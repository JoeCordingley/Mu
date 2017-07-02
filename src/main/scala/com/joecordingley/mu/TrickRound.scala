package com.joecordingley.mu

import cats.data.State
import cats.data.State._
import com.joecordingley.mu.CardPlayRound.TrickAsPlayed
import monocle.{Lens, Traversal}
import monocle.macros.GenLens
import Util._

import scala.collection.immutable.ListMap

/**
  * Created by joe on 01/07/17.
  */
object TrickRound {

  val trickLens:Lens[TrickState,TrickAsPlayed] = GenLens[TrickState](_.trick)
  val allPlayersLens: Lens[TrickState,Set[Player]] = GenLens[TrickState](_.allPlayers)
  val eachPlayer: Traversal[TrickState, Player] =
    allPlayersLens composeTraversal setTraversal

  def update(event: CardPlayEvent): (TrickState) => TrickState =
    trickLens.modify(_ + (event.player -> event.card))
  def updateState(event: CardPlayEvent): State[TrickState,Unit] =
    modify[TrickState](update(event))

  def updatePlayers(event: CardPlayEvent): (TrickState) => TrickState =
    eachPlayer.modify(_.update(event))
  def updatePlayersState(event: CardPlayEvent): State[TrickState,Unit] =
    modify[TrickState](updatePlayers(event))


  def nextPlayer: (TrickState) => TrickState = playersLeftLens.modify(_.tail)
  def nextPlayerState:State[TrickState,Player] = for {
    state <- get[TrickState]
    currentPlayer = state.currentPlayer
    _ <- modify[TrickState](nextPlayer)
  } yield currentPlayer

  val playersLeftLens: Lens[TrickState,List[Player]] = GenLens[TrickState](_.playersLeft)

  def getCard(player: Player): State[TrickState,Card] =
    pure[TrickState,Card](player.getCard)

  def trick:State[TrickState,FinishedTrick] = for {
    trickState <- get[TrickState]
    finishedTrick <- if (trickState.isFinished)
      pure[TrickState,FinishedTrick](FinishedTrick.fromTrickState(trickState)) else for {
      player <- nextPlayerState
      card <- getCard(player)
      event = CardPlayEvent(player,card)
      _ <- updateState(event)
      _ <- updatePlayersState(event)
      finishedTrick <- trick
    } yield finishedTrick
  } yield finishedTrick

}
case class FinishedTrick(winner:Player,trick:List[Card])
case class TrickState(trick:TrickAsPlayed, trumps:Trumps, playersLeft:List[Player], allPlayers:Set[Player]){

  val isFinished:Boolean = playersLeft.isEmpty
  val currentPlayer:Player = playersLeft.head
}
object TrickState {
  def apply(players: List[Player],firstPlayer:Player,trumps: Trumps):TrickState = {
    val trick = ListMap.empty[Player,Card]
    val playerOrder = Players.makeThisPlayerFirst(firstPlayer)(players)
    TrickState(trick,trumps,playerOrder,players.toSet)
  }
  def fromFinishedTrick(players: List[Player],finishedTrick: FinishedTrick,trumps: Trumps):TrickState = {
    val firstPlayer = finishedTrick.winner
    TrickState(players,firstPlayer,trumps)
  }
}
object FinishedTrick{

  def fromTrickState(trickState: TrickState):FinishedTrick = {
    val trickAsPlayed = trickState.trick
    val trumps = trickState.trumps
    val winner = trickWinner(trickAsPlayed,trumps)
    val trickWon = trickAsPlayed.values.toList
    FinishedTrick(winner,trickWon)
  }

  def trickWinner(trick:TrickAsPlayed, trumps:Trumps) = {
    val (_,cardLed) = trick.head
    implicit val ordering = CardOrdering(trumps,cardLed)
    val (player,_) = trick.maxBy {
      case (_,card)=> card
    }
    player
  }

}
case class CardPlayEvent(player: Player,card: Card) extends Event
