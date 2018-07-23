package com.joecordingley.mu

import cats.free.Free

sealed trait CardPlayStatus
sealed trait CardPlayScores
case class FinishedCardPlay(scores: CardPlayScores) extends CardPlayStatus
sealed trait TrickPlayStatus
case object UnfinishedCardPlay extends CardPlayStatus
case object UnfinishedTrickPlay extends TrickPlayStatus
case class FinishedTrickPlay(winner: Player) extends TrickPlayStatus

object CardPlayRound {

  type CardPlayFree[A] = Free[CardPlayADT, A]

  def getStatus: CardPlayFree[CardPlayStatus] = ???
  def getTrickStatus: CardPlayFree[TrickPlayStatus] = ???
  def finishCardPlay(scores: CardPlayScores): CardPlayFree[CardPlayScores] =
    Free.pure(scores)
  def finishTrickPlay: CardPlayFree[Unit] = Free.pure(())
  def currentPlayer: CardPlayFree[Player] = ???
  def getCard(player: Player): CardPlayFree[Card] = ???
  def playCard(card: Card): CardPlayFree[Unit] = ???

  def play: CardPlayFree[CardPlayScores] =
    for {
      status <- getStatus
      finish <- status match {
        case FinishedCardPlay(scores) => finishCardPlay(scores)
        case UnfinishedCardPlay       => playTrick.flatMap(_ => play)
      }
    } yield finish

  def playTrick: CardPlayFree[Unit] =
    for {
      status <- getTrickStatus
      _ <- status match {
        case _: FinishedTrickPlay => finishTrickPlay
        case UnfinishedTrickPlay =>
          getCardFromNextPlayer.flatMap(_ => playTrick)
      }
    } yield ()

  def getCardFromNextPlayer: CardPlayFree[Unit] =
    for {
      player <- currentPlayer
      card <- getCard(player)
      _ <- playCard(card)
    } yield ()

}
