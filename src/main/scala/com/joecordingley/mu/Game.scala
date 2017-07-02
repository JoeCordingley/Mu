package com.joecordingley.mu

import cats.data.State
import State._

/**
  * Created by joe on 30/05/17.
  */
sealed trait EndCondition
case class Score(goal: Int) extends EndCondition
case class Rounds(count:Int) extends EndCondition
object Game{

  type Scores = Map[Player,Int]
  def playGame(endCondition: EndCondition, players: List[Player]) : State[GameState,Scores] = ???
  def playRound(players:List[Player]): State[CardRoundState,Scores] = ???
}
case class GameState()
case class Trumps(major:Trump,minor:Trump)
sealed trait Trump
case class SuitTrump(suit: Suit) extends Trump
case class NumberTrump(rank:Int) extends Trump
case object NoTrump extends Trump

object CardOrdering {

  def cardIsOfLedSuit(cardLed:Card)(thisCard:Card): Boolean = cardLed.suit == thisCard.suit
  def cardIsDoubleTrump(trumps: Trumps)(card: Card): Boolean =
    cardIsMajorTrump(trumps)(card) && cardIsMinorTrump(trumps)(card)
  def cardIsMajorTrump(trumps: Trumps): (Card) => Boolean = cardIsTrump(trumps.major)
  def cardIsMinorTrump(trumps: Trumps): (Card) => Boolean = cardIsTrump(trumps.minor)

  def cardIsTrump(trump: Trump)(card:Card):Boolean = trump match {
    case SuitTrump(suit) => card.suit == suit
    case NumberTrump(rank) => card.rank == rank
    case NoTrump => false
  }

  def apply(trumps: Trumps,cardLed:Card): Ordering[Card] = Ordering.by{card =>
    (
      cardIsMajorTrump(trumps)(card),
      cardIsMinorTrump(trumps)(card),
      cardIsOfLedSuit(cardLed)(card),
      card.rank
    )
  }
}

