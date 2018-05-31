package com.joecordingley.mu

import cats.data.StateT
import cats.data.StateT._
import cats.data._
import cats.effect.IO
import cats._
import cats.free.Free
import cats.free.Free.liftF
import cats.implicits._



sealed trait AuctionADT[A]

case class PlaceBid(bid:Bid) extends AuctionADT[Unit]
case object GetStatus extends AuctionADT[AuctionStatus]
case class GetTrump(player:Player) extends AuctionADT[Trump]
case class GetPartner(player:Player) extends AuctionADT[Player]
case object NextPlayer extends AuctionADT[Player]
case class GetBid(player:Player) extends AuctionADT[Bid]

object AuctionPlay {

  type AuctionFree[A] = Free[AuctionADT, A]

  def placeBid(bid: Bid): AuctionFree[Unit] = liftF(PlaceBid(bid))
  def getStatus: AuctionFree[AuctionStatus] = liftF(GetStatus)
  def getTrump(player:Player): AuctionFree[Trump] = liftF(GetTrump(player))
  def getPartner(player:Player): AuctionFree[Player] = liftF(GetPartner(player))
  def nextPlayer: AuctionFree[Player] = liftF(NextPlayer)
  def getBid(player: Player): AuctionFree[Bid] = liftF(GetBid(player))

  def getBidFromNextPlayer: AuctionFree[Unit] = for {
    player <- nextPlayer
    bid <- getBid(player)
    _ <- placeBid(bid)
  } yield ()

  def finishAuction(finish:FinishedAuctionRound): AuctionFree[FinishedAuctionRound] = Free.pure(finish)

  def play: AuctionFree[FinishedAuctionRound] = for {
    status <- getStatus
    finish <- status match {
      case Finished(outcome) => outcome match {
        case f: FinishedAuctionRound => finishAuction(f)
        case ChiefAndVice(chief, vice) => for {
          viceTrump <- getTrump(vice)
          chiefTrump <- getTrump(chief)
          partner <- getPartner(chief)
        } yield TwoTrumps(chief,chiefTrump,partner,viceTrump)
        case ChiefOnly(chief) => for {
          trump <- getTrump(chief)
          partner <- getPartner(chief)
        } yield OneTrump(chief,trump,partner)
        case ChiefThreePlayers(chief) => getTrump(chief).map(ThreePlayerFinish(chief,_))
      }
      case Unfinished => getBidFromNextPlayer.flatMap(_ => play)
    } 
  } yield finish

}
