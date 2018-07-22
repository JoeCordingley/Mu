package com.joecordingley.mu

import cats.effect.IO
import scala.collection.SortedMap
import cats.implicits._

object CLI extends AvailabilityDefinedInteractions[IO] {
  def putLine(s:String) = IO(println(s))
  val readLine = IO(scala.io.StdIn.readLine)
  def availableCardsMap(s:Set[Card]):SortedMap[Int,Card] = SortedMap(s.toList.zipWithIndex.map(_.swap):_*)

  def availableCardsString(playerString:String, availableCards:SortedMap[Int,Card]) = s"player $playerString, choose from these cards\n" + 
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

  def askPlayerForBid(player:Player,availableCards:Set[Card]):IO[Bid] = {
    val map = availableCardsMap(availableCards)
    val validInts = map.keys.toSet
    val prompt = availableCardsString(player.id.toString, map)

    val question = for {
      _ <- putLine(prompt)
      userString <- readLine
    } yield parseUserString(userString).filter(_  subsetOf validInts)

    keepAsking(question)
      .map(ints => map.filterKeys(ints).values.toSet)
      .map{s => if (s.isEmpty) Pass else Raise(s) }
  }
  def askPlayerForPartner(player:Player, availablePartners:Set[Player]):IO[Player] = ???
}

trait AvailabilityDefinedInteractions[F[_]] {

  def askPlayerForBid(player:Player, availableCards:Set[Card]):F[Bid]
  def askPlayerForPartner(player:Player, availablePartners:Set[Player]):F[Player]

}
