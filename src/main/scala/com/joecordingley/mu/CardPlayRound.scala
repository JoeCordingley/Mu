package com.joecordingley.mu

import cats.data.State
import cats.data.State._
import com.joecordingley.mu.Game.Scores
import monocle.Lens
import monocle.macros.GenLens

import scala.collection.immutable.ListMap

/**
  * Created by joe on 23/06/17.
  */
object CardPlayRound {
  type TrickAsPlayed = ListMap[Player,Card]
  type TrickWon = List[Card]
  type WonTricks = List[TrickWon]

  def play:State[CardRoundState,Scores] = for {
    roundState <- get[CardRoundState]
    scores <- if(roundState.isFinished)
      pure[CardRoundState,Scores](roundState.getScores)
    else for {
      finishedTrick <- playTrick
      _ <- updateState(finishedTrick)
      scores <- play
    } yield scores
  } yield scores

  val finishedTricksLens: Lens[CardRoundState,List[FinishedTrick]] = GenLens[CardRoundState](_.tricks)
  def addTrick(trick:FinishedTrick)(tricks:List[FinishedTrick]):List[FinishedTrick] = trick :: tricks

  def playTrick:State[CardRoundState,FinishedTrick] = TrickRound.trick.transformS(initialTrickState,(cardRoundState,_) => cardRoundState )

  def initialTrickState(cardRoundState: CardRoundState): TrickState = {
    import cardRoundState._
    if (cardRoundState.isFirstRound) TrickState(players,firstPlayer,trumps)
    else TrickState.fromFinishedTrick(players,lastTrickPlayed.get,trumps)
  }
  def update(finishedTrick: FinishedTrick) = finishedTricksLens.modify(addTrick(finishedTrick))
  def updateState(trick: FinishedTrick):State[CardRoundState,Unit] = modify[CardRoundState](update(trick))



}
object CardRoundState {
  def tricksToBePlayed(players:Int) = Deck.Full.size / players
  def fromFinshedBids(finishedBids: FinishedBids):CardRoundState = ???
}


case class CardRoundState(players:List[Player],tricks:List[FinishedTrick],chief:Player,partner:Player,trumps:Trumps) {
  import CardRoundState._
  val isFinished:Boolean = tricks.size == tricksToBePlayed(players.size)
  val isFirstRound:Boolean = tricks.isEmpty
  def addTrickScores(tricks:List[FinishedTrick]) = ???
  def firstPlayer:Player = ???
  def lastTrickPlayed:Option[FinishedTrick] = tricks.headOption

  def getScores:Scores = ???
}
