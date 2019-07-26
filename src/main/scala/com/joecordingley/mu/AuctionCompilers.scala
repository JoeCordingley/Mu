package com.joecordingley.mu

import cats._
import cats.data._
import cats.effect.IO

object AuctionCompilers {
  type AuctionState[A] = State[AuctionObject, A]
  type AuctionStateIO[A] = StateT[IO, AuctionObject, A]

  val AuctionStateCompiler = new (AuctionStateADT ~> AuctionState) {
    import cats.data.State._
    override def apply[A](fa: AuctionStateADT[A]) = fa match {
      case PlaceBid(bid) =>
        modify[AuctionObject] {
          case s: UnfinishedAuction => s.bid(bid)
          case _: FinishedAuctionObject => throw new Exception("is finished")
        }
      case GetStatus =>
        inspect[AuctionObject, AuctionStatus] {
          case FinishedAuctionObject(_, outcome, _) =>
            FinishedAuctionStatus(outcome)
          case _: UnfinishedAuction => UnfinishedAuctionStatus
        }
      case NextPlayer => inspect[AuctionObject, Player](_.state.currentPlayer)
      case SetTrump(trump) =>
        modify[AuctionObject] {
          case s: FinishedAuctionObject => s.setTrump(trump)
          case _ => throw new Exception("Is not finished")
        }
    }
  }

  val CLICompiler = new (PlayerADT ~> AuctionStateIO) {
    type AuctionObjectIO[A] = AuctionObject => IO[A]
    override def apply[A](fa: PlayerADT[A]): AuctionStateIO[A] =
      StateT.inspectF(inner(fa)(_))

    def inner[A](fa: PlayerADT[A]): AuctionObjectIO[A] = fa match {
      case GetBid(player) =>
        auctionObject =>
          CLI.askPlayerForBid(
            player,
            auctionObject.state.cardsInHand(player),
            auctionObject.state.maxBidAllowedForCurrentPlayer)
      case GetPartner(player) =>
        auctionObject =>
          CLI.askPlayerForPartner(player, auctionObject.state.validPartners)
      case GetTrump(player) =>
        auctionObject =>
          CLI.askPlayerForTrump(
            player,
            auctionObject.state.availableTrumpsFor(player))

    }
  }

  val FullCompiler = new (AuctionADT ~> AuctionStateIO) {
    override def apply[A](fa: AuctionADT[A]): AuctionStateIO[A] = fa match {
      case p: PlayerADT[A] => CLICompiler(p)
      case a: AuctionStateADT[A] =>
        AuctionStateCompiler(a).transformF(eval => IO.pure(eval.value))
    }
  }

}
