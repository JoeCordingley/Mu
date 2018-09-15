package com.joecordingley.mu

import cats.effect.IO
import scala.collection.SortedMap
import cats.implicits._
import cats._

object CLI {
  implicit val CardShow = Show.show[Card] {
    case Card(suit, rank) => s"$suit ${rank.value}"
  }
  implicit val PlayerShow = Show.show[Player](_.id.toString)
  implicit val TrumpShow = Show.fromToString[Trump]
  def putLine(s: String) = IO(println(s))
  val readLine = IO(scala.io.StdIn.readLine)
  def toSortedMap[V](s: Set[V]): SortedMap[Int, V] =
    SortedMap(s.toList.zipWithIndex.map(_.swap): _*)

  def availabilityString[A: Show](playerString: String,
                                  choices: SortedMap[Int, A]) =
    s"player $playerString, choose from\n" + choices
      .map {
        case (k, c) => s"$k: ${c.show}"
      }
      .mkString("\n")

  def parseCommaSeparatedInts(s: String): Option[Set[Int]] =
    if (s.isEmpty) Some(Set.empty)
    else s.split(",").toList.traverse(toInt _).map(_.toSet)

  def toInt(s: String): Option[Int] =
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }

  def keepAsking[A](io: IO[Option[A]]): IO[A] = io.flatMap {
    case Some(a) => IO.pure(a)
    case None    => keepAsking(io)
  }

  def askPlayerForBid(player: Player,
                      availableCards: Set[Card],
                      maxBidSize: Int): IO[Bid] = {
    val map = toSortedMap(availableCards)
    val validInts = map.keys.toSet
    val parseFunction: String => Option[Bid] = parseCommaSeparatedInts(_)
      .filter(_ subsetOf validInts)
      .filter(_.size <= maxBidSize)
      .map(map.filterKeys(_).values.toSet)
      .map { s =>
        if (s.isEmpty) Pass else Raise(s)
      }
    askPlayerForX(player, map, parseFunction)
  }
  def askPlayerForX[X, C: Show](player: Player,
                                map: SortedMap[Int, C],
                                parseFunction: String => Option[X]): IO[X] = {
    val prompt = availabilityString(player.show, map)
    val question = for {
      _ <- putLine(prompt)
      userString <- readLine
    } yield parseFunction(userString)
    keepAsking(question)
  }

  private def askPlayerSimple[X: Show](player: Player, xs: Set[X]): IO[X] = {
    val map = toSortedMap(xs)
    val parseFunction: String => Option[X] = toInt(_).flatMap(map.get)
    askPlayerForX(player, map, parseFunction)
  }

  def askPlayerForPartner(player: Player,
                          availablePartners: Set[Player]): IO[Player] =
    askPlayerSimple(player, availablePartners)

  def askPlayerForTrump(player: Player,
                        availableTrumps: Set[Trump]): IO[Trump] =
    askPlayerSimple(player, availableTrumps)

  def askPlayerForCard(player: Player, availableCards: Set[Card]): IO[Card] =
    askPlayerSimple(player, availableCards)
}
