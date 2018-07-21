package com.joecordingley.mu
import Auction._
import com.joecordingley.mu.Util._

sealed trait Bid
case object Pass extends Bid
case class Raise(cards:List[Card]) extends Bid

object Auction {
 

  def initial(players:Vector[Player]):UnfinishedAuction = new UnfinishedAuction(AuctionState(players,Vector()))

  val viceOrdering:Ordering[List[Card]] = {
    val sizeOrdering = Ordering.by[List[Card],Int](_.size)
    def orderingForRank(rank:Int) = Ordering.by[List[Card],Int](_.filter(_.rank == rank).size) 
    val viceOrderingsInOrder = sizeOrdering :: (9 to 1 by -1).map(orderingForRank).toList
    Util.orderingFromOrderings(viceOrderingsInOrder).reverse 
  }

}


//case class Finished(finishedBidRound: BidRoundOutcome) extends AuctionStatus

sealed trait ResolvedAuction
sealed trait AuctionOutcome
case class FinishedAuctionObject(state: AuctionState, outcome: AuctionOutcome) extends AuctionObject
case class AuctionState(players: Vector[Player], bids:Vector[Bid]) {
  import Auction._
  val playerCount = players.size
  val playerSequence = Stream.continually(players).flatten
  val playerBids = playerSequence.zip(bids).toList
  val playerBidsReversed = playerBids.reverse
  val totals = {
    val empties:Map[Player,List[Card]] = players.map(_ -> Nil).toMap
    playerBids.foldLeft(empties){
      case (acc, (player,Raise(cards))) => acc.updated(player, acc(player) ::: cards)
      case (acc, _) => acc
    }
  }
  val maxBid:Int = totals.values.map(_.size).max
  val leaders:Set[Player] = totals.collect{
    case (player,bid) if bid.size == maxBid => player
  }.toSet
  val lastOfLeadersToRaise:Option[Player] = if (leaders.isEmpty) None 
    else playerBidsReversed.collectFirst{
      case (player, _:Raise) if leaders(player) => player
    }

  def isFinished:Boolean = if (bids.size < playerCount ) false else bids.reverse.take(playerCount).forall(_.isInstanceOf[Pass.type])

  val vice:Option[Player] = leaders match {
    case SingleElementSet(leader) => ( totals - leader ).toList.outrightFirst(viceOrdering.on(_._2)).map(_._1)
    case _ => None
  } 
  
}
case class TwoTrumps(chief:Player,chiefTrump:Trump,partner:Player,viceTrump:Trump) extends ResolvedAuction
case class OneTrump(chief:Player,chiefTrump:Trump,partner:Player) extends ResolvedAuction
case class ThreePlayerFinish(chief:Player,chiefTrump:Trump) extends ResolvedAuction

case class ChiefAndVice( chief:Player,vice:Player) extends AuctionOutcome
case class ChiefOnly( chief:Player) extends AuctionOutcome
case class ChiefThreePlayers(chief:Player) extends AuctionOutcome
case class Eklat(lastPlayed: Player, othersOnEqual: Set[Player]) extends AuctionOutcome with ResolvedAuction 
case object EklatNoPoints extends AuctionOutcome with ResolvedAuction


sealed trait AuctionObject {
  val state: AuctionState
}

case class UnfinishedAuction(state: AuctionState) extends AuctionObject {
  def bid(b:Bid):AuctionObject = {
    val newBids = state.bids:+b
    val newStatus = state.copy(bids = newBids)
    if (!newStatus.isFinished) UnfinishedAuction(newStatus)
    else newStatus.leaders match {
      case SingleElementSet(chief) if state.playerCount == 3 => FinishedAuctionObject(newStatus,ChiefThreePlayers(chief))
      case SingleElementSet(chief)  => newStatus.vice match {
        case Some(vice) => FinishedAuctionObject(newStatus,ChiefAndVice(chief,vice))
        case None => FinishedAuctionObject(newStatus,ChiefOnly(chief))
      }
      case leaders => newStatus.lastOfLeadersToRaise match {
        case Some(offender) => FinishedAuctionObject(newStatus, Eklat(offender,leaders - offender))
        case None => FinishedAuctionObject(newStatus,EklatNoPoints)
      }
    }
  }

}
