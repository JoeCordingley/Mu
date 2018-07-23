package com.joecordingley.mu

import cats._
import cats.data._
import cats.effect.IO

object AuctionStateCompiler {
  import cats.data.State._
  type AuctionState[A] = State[AuctionObject, A]
  type AuctionPF = PartialFunction[AuctionObject, AuctionObject]

  def placeBid(b: Bid): AuctionState[Unit] = modify {
    val pf: AuctionPF = {
      case s: UnfinishedAuction => s.bid(b)
    }
    pf orElse isFinished
  }

  private def isFinished: PartialFunction[AuctionObject, Nothing] = {
    case _: FinishedAuctionObject => throw new Exception("is finished")
  }

//  def getBid(player:Player):CLIAuction[Bid] = StateT.inspectF{ o =>
//    val availableCards = o.state.cardsInHand(player)
//    CLI.askPlayerForBid(availableCards)
//  }

  //def askPlayerForBid(availableCards:Set[Card]):IO[Set[Card]] =
  val getStatus: AuctionState[AuctionStatus] = inspect {
    case FinishedAuctionObject(_, outcome) => FinishedAuctionStatus(outcome)
    case _: UnfinishedAuction              => UnfinishedAuctionStatus
  }

  val nextPlayer: AuctionState[Player] = inspect(_.state.currentPlayer)

  val compiler = new (AuctionStateADT ~> AuctionState) {
    override def apply[A](fa: AuctionStateADT[A]) = fa match {
      case PlaceBid(b) => placeBid(b)
      case GetStatus   => getStatus
      case NextPlayer  => nextPlayer
    }
  }

}

trait AuctionPlayerIOCompiler {
  import StateT._

  type AuctionState[A] = StateT[IO, AuctionObject, A]
  type AuctionObjectFunction[A] = AuctionObject => IO[A]

  val ioCompiler: PlayerADT ~> AuctionObjectFunction

  val compiler = new (PlayerADT ~> AuctionState) {
    override def apply[A](fa: PlayerADT[A]): AuctionState[A] =
      inspectF(auctionObject => ioCompiler(fa)(auctionObject))
  }

}
