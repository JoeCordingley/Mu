package com.joecordingley.mu

import cats.data.State
import State._

/**
  * Created by joe on 30/05/17.
  */
sealed trait EndCondition
case class Score(goal: Int) extends EndCondition
case class Rounds(count: Int) extends EndCondition
object Game {

  def deal(players: List[Player]): List[(Player, Set[Card])] = {
    import scala.util.Random._
    val numberOfPlayers = players.size
    val deck = numberOfPlayers match {
      case 3         => Deck.Reduced
      case 4 | 5 | 6 => Deck.Full
      case _         => throw new Exception("invalid amt of players")
    }
    players.zip(
      shuffle(deck.toList)
        .grouped(deck.size / numberOfPlayers)
        .map(_.toSet)
        .toList)
  }

  type Scores = Map[Player, Int]
}
case class GameState()
case class Trumps(major: Trump, minor: Trump)
sealed trait Trump
case class SuitTrump(suit: Suit) extends Trump
case class NumberTrump(rank: Int) extends Trump
case object NoTrump extends Trump
