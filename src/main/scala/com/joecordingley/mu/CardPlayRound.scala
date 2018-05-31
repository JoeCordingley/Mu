package com.joecordingley.mu

import cats.free.Free
import cats.InjectK
import cats.arrow.FunctionK

sealed trait CardPlayADT[A]
sealed trait CardPlayStatus
trait FinishedCardPlay extends CardPlayStatus
case object UnfinishedF extends CardPlayStatus with TrickPlayStatus

object CardPlayRound {

  type CardPlayFree[A] = Free[CardPlayADT,A]

  def getStatus:CardPlayFree[CardPlayStatus] = ???
  def playTrick:CardPlayFree[FinishedTrickPlay] = TrickPlay.play mapK (FunctionK lift identity)
  def finishCardPlay(f:FinishedCardPlay):CardPlayFree[FinishedCardPlay] = Free.pure(f)

  def play: CardPlayFree[FinishedCardPlay] = for {
    status <- getStatus
    finish <- status match {
      case f:FinishedCardPlay => finishCardPlay(f)
      case UnfinishedF => playTrick.flatMap(_ => play)
    }
  } yield finish

}

sealed trait TrickPlayADT[A] extends CardPlayADT[A]
sealed trait TrickPlayStatus
trait FinishedTrickPlay extends TrickPlayStatus

object TrickPlay {

  type TrickPlayFree[A] = Free[TrickPlayADT,A]

  def getStatus:TrickPlayFree[TrickPlayStatus] = ???
  def getCard:TrickPlayFree[Unit] = ???
  def finishTrickPlay(f:FinishedTrickPlay):TrickPlayFree[FinishedTrickPlay] = Free.pure(f)

  def play: TrickPlayFree[FinishedTrickPlay] = for {
    status <- getStatus
    finish <- status match {
      case f:FinishedTrickPlay => finishTrickPlay(f)
      case UnfinishedF => getCard.flatMap(_ => play)
    }
  } yield finish

}
