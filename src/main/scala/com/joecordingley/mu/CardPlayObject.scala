package com.joecordingley.mu

import CardPlayObject._

object CardPlayObject{
  type Trick = Vector[Card]
}

sealed trait TrickObject
case class FinishedTrick(players: Vector[Player], trick:Trick, trumps:Trumps) 
extends TrickObject {
  val cardLed = trick.head
  implicit val ordering = CardOrdering(trumps,cardLed)
  val winner = ( players zip trick sortBy (_._2)).head._1
}

case class UnfinishedTrick(players:Vector[Player], trick: Trick, trumps:Trumps) extends TrickObject {
  val numberOfCardsPlayed = trick.size 
  val currentPlayer = players(numberOfCardsPlayed)
  val playerCount = players.size
  def playCardToTrick(card: Card): TrickObject = {
    val newTrick = trick :+ card
    if (newTrick.size == playerCount) FinishedTrick(players, newTrick, trumps)
    else this.copy(trick = newTrick)
  } 

}

class CardPlayObject(players: Vector[Player], tricks: Vector[Trick]) {
  
}
