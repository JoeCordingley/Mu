package com.joecordingley.mu

import cats.free.Free
import cats.free.Free.liftF
import cats.implicits._
import cats._
import com.joecordingley.mu.CardPlayObject._

case class WonTrick(winner: Player, trick: Trick)
case class PlayedCard(player: Player, card: Card)

sealed trait CardPlayADT[A]
sealed trait CardPlayGameADT[A] extends CardPlayADT[A]

case class GetCard(player: Player) extends CardPlayADT[Card]
case class PlayCard(card: Card) extends CardPlayGameADT[Unit]
case class GetPlayersStartingFrom(player: Player)
    extends CardPlayGameADT[List[Player]]
case class GetWinner(trick: List[PlayedCard]) extends CardPlayGameADT[Player]
case class GetScores(wonTricks: List[WonTrick])
    extends CardPlayGameADT[CardPlayScores]
case class RecordTrick(trick: TrickAsPlayed) extends CardPlayGameADT[Unit]

object CardPlayRound {

  type CardPlayFree[A] = Free[CardPlayADT, A]

  def getCard(player: Player): CardPlayFree[Card] = liftF(GetCard(player))
  def playCard(card: Card): CardPlayFree[Unit] = liftF(PlayCard(card))
  def getPlayersStartingFrom(player: Player): CardPlayFree[List[Player]] =
    liftF(GetPlayersStartingFrom(player))
  def getWinner(trick: List[PlayedCard]): CardPlayFree[Player] =
    liftF(GetWinner(trick))
  def getScores(wonTricks: List[WonTrick]): CardPlayFree[CardPlayScores] =
    liftF(GetScores(wonTricks))
  def recordTrick(trick: TrickAsPlayed): CardPlayFree[Unit] =
    liftF(RecordTrick(trick))

  def playTrick(startingPlayer: Player): CardPlayFree[WonTrick] =
    for {
      players <- getPlayersStartingFrom(startingPlayer)
      trick <- players.traverse(getCardFromPlayer)
      winner <- getWinner(trick)
      wonTrick = WonTrick(winner, trick.map(_.card))
      _ <- recordTrick(trick)
    } yield wonTrick

  def play(startingPlayer: Player,
           numberOfTricks: Int): CardPlayFree[CardPlayScores] =
    for {
      wonTricks <- playNTricks(numberOfTricks, startingPlayer)
      scores <- getScores(wonTricks)
    } yield scores

  def playNTricks(n: Int,
                  startingPlayer: Player): CardPlayFree[List[WonTrick]] = {
    val init: Player => CardPlayFree[List[WonTrick]] = _ => Free.pure(Nil)
    val f = (1 to n).foldRight(init) {
      case (_, acc) =>
        player =>
          for {
            wonTrick <- playTrick(player)
            winner = wonTrick.winner
            tricks <- acc(winner)
          } yield wonTrick :: tricks
    }
    f(startingPlayer)
  }

  def getCardFromPlayer(player: Player): CardPlayFree[PlayedCard] =
    for {
      card <- getCard(player)
      _ <- playCard(card)
    } yield PlayedCard(player, card)

}
