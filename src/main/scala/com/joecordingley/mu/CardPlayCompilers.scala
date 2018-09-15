package com.joecordingley.mu

import cats.data.State
import cats.effect.IO
import cats.data.StateT
import cats.~>
import com.joecordingley.mu.CardPlayObject._

object CardPlayCompilers {

  type CardPlayState[A] = State[CardPlayObject, A]

  val StateCompiler = new (CardPlayGameADT ~> CardPlayState) {
    override def apply[A](fa: CardPlayGameADT[A]): CardPlayState[A] = fa match {
      case PlayCard(card: Card) => State.modify[CardPlayObject](_.playCard(card))
      case GetPlayersStartingFrom(player: Player) =>
        State.inspect[CardPlayObject, List[Player]](_.getPlayersStartingFrom(player))
      case GetScores(wonTricks: List[WonTrick]) =>
        State.inspect(_.getScores(wonTricks))
      case GetWinner(trick: List[PlayedCard]) => State.inspect(_.getWinner(trick))
      case RecordTrick(trick: TrickAsPlayed) => State.modify(_.recordTrick(trick))
    }
  }

  //def GetCardCLI(player: Player): StateT[IO, CardPlayObject, Card] = StateT.inspectF()

}
