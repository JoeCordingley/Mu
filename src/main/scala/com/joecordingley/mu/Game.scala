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

  type Scores = Map[Player, Int]
}
case class GameState()
case class Trumps(major: Trump, minor: Trump)
sealed trait Trump
case class SuitTrump(suit: Suit) extends Trump
case class NumberTrump(rank: Int) extends Trump
case object NoTrump extends Trump
