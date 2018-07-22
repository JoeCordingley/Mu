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
sealed trait PlayerADT[A] extends AuctionADT[A]
sealed trait AuctionStateADT[A] extends AuctionADT[A]

case class PlaceBid(bid:Bid) extends AuctionStateADT[Unit]
case object GetStatus extends AuctionStateADT[AuctionStatus]
case class GetTrump(player:Player) extends PlayerADT[Trump]
case class GetPartner(player:Player) extends PlayerADT[Player]
case object NextPlayer extends AuctionStateADT[Player]
case class GetBid(player:Player) extends PlayerADT[Bid]
sealed trait AuctionStatus
case object UnfinishedAuctionStatus extends AuctionStatus
case class FinishedAuctionStatus(outcome: AuctionOutcome) extends AuctionStatus

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

  def finishAuction(finish:ResolvedAuction): AuctionFree[ResolvedAuction] = Free.pure(finish)

  def play: AuctionFree[ResolvedAuction] = for {
    status <- getStatus
    finish <- status match {
      case FinishedAuctionStatus(outcome) => outcome match {
        case resolved: ResolvedAuction => finishAuction(resolved)
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
      case UnfinishedAuctionStatus => getBidFromNextPlayer.flatMap(_ => play)
    } 
  } yield finish

}
