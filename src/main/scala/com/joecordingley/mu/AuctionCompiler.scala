package com.joecordingley.mu

import cats._
import cats.data._
import cats.implicits._
import cats.data.StateT._
import cats.effect.IO
import scala.collection.SortedMap

object AuctionCompiler {
  type CLIAuction[A] = StateT[IO,AuctionObject,A]
  def placeBid(b:Bid):CLIAuction[Unit] = StateT.modify( orThrow{
    case s: UnfinishedAuction => s.bid(b)
  } )  

  private def isFinished: PartialFunction[AuctionObject,Nothing] = {
    case _: FinishedAuctionObject => throw new Exception("is finished")
  }
  private def orThrow[A](pf:PartialFunction[AuctionObject,A]):PartialFunction[AuctionObject,A] = pf orElse isFinished

  def getBid(player:Player):CLIAuction[Bid] = StateT.inspectF{ o =>
    val availableCards =o.state.cardsInHand(player)
    CLI.askPlayerForBid(availableCards)
  }

  //def askPlayerForBid(availableCards:Set[Card]):IO[Set[Card]] = 

  val cliCompiler  = new (AuctionADT ~> CLIAuction) {
    override def apply[A](fa:AuctionADT[A]) = fa match {
      case PlaceBid(b) => placeBid(b)
      case GetBid(player) => getBid(player)
    }
  }
  
}
object CLI {
  def putLine(s:String) = IO(println(s))
  val readLine = IO(scala.io.StdIn.readLine)
  def availableCardsMap(s:Set[Card]):SortedMap[Int,Card] = SortedMap(s.toList.zipWithIndex.map(_.swap):_*)

  def availableCardsString(availableCards:SortedMap[Int,Card]) = "choose from these cards\n" + 
  availableCards.map{
    case (k,Card(suit,rank)) => s"$k: $suit ${rank.value}"
  }.mkString("\n")
  def parseUserString(s:String):Option[Set[Int]] = s.split(",").toList.traverse(toInt _).map(_.toSet)
  def toInt(s:String):Option[Int] = try {
    Some(s.toInt)
  } catch {
    case e: Exception => None
  }

  def keepAsking[A](io:IO[Option[A]]):IO[A] = io.flatMap{
    case Some(a) => IO.pure(a)
    case None => keepAsking(io)
  }

  def askPlayerForBid(availableCards:Set[Card]):IO[Bid] = {
    val map = availableCardsMap(availableCards)
    val validInts = map.keys.toSet
    val prompt = availableCardsString(map)
    val question = for {
      _ <- putLine(prompt)
      userString <- readLine
    } yield parseUserString(userString).filter(_  subsetOf validInts)
    keepAsking(question)
      .map(ints => map.filterKeys(ints).values.toSet)
      .map{s => if (s.isEmpty) Pass else Raise(s) }
  }
}
