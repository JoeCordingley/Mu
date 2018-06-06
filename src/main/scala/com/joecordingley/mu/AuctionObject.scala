package com.joecordingley.mu


sealed trait Bid
case object Pass extends Bid
case class Raise(cards:List[Card]) extends Bid

object Auction {
 

  def initial(players:Vector[Player]):UnfinishedAuction = new UnfinishedAuction(players,Vector())

  val viceOrdering:Ordering[List[Card]] = {
    val sizeOrdering = new Ordering[List[Card]]{
      override def compare(x:List[Card],y:List[Card]):Int = x.size - y.size
    }
    def orderingForRank(rank:Int) = new Ordering[List[Card]]{
      override def compare(x:List[Card],y:List[Card]):Int = x.filter(_.rank == rank).size - y.filter(_.rank == rank).size
    }
    val viceOrderingsInOrder = sizeOrdering :: (9 to 1 by -1).map(orderingForRank).toList
    Util.orderingFromOrderings(viceOrderingsInOrder).reverse 
  }
}


//case class Finished(finishedBidRound: BidRoundOutcome) extends AuctionStatus

sealed trait ResolvedAuction
sealed trait AuctionOutcome
case class FinishedAuction(history: AuctionState, outcome: AuctionOutcome)
case class AuctionState(players: Vector[Player], bids:Vector[Player]) {
  val playerCount = players.size
  val playerSequence = Stream.continually(players).flatten
  val playerBidsReversed = playerSequence.zip(bids).toList.reverse
  def allArePasses:Boolean = bids.forall( _.isInstanceOf[Pass.type] )

  val totalsOrderedByLastPlayed = {
    val empties: List[(Player,List[Card])] = playerBidsReversed.take(playerCount).map{
      case (player,_) => (player, List[Card]())
    }
    playerBidsReversed.grouped(playerCount).foldLeft(empties){
      case (playerTotals,bids) => playerTotals zip bids.padTo(playerCount,Pass) map {
        case ((player,total),Raise(cards)) => (player,cards:::total)
        case (playerTotal,_) => playerTotal
      }
    }
  }
  def isFinished:Boolean = bids.reverse.take(playerCount).forall(_.isInstanceOf[Pass.type])
  val leadersOrderedByLastPlayed = {
    val maxBid: Int = totalsOrderedByLastPlayed.map{
      case (_,cards) => cards.size
    }.max
    totalsOrderedByLastPlayed.collect{
      case (player,cards) if cards.size == maxBid => player
    }
  }

  val currentVice:Option[Player] = leadersOrderedByLastPlayed match {
    case List(leader) => ( totalBids - leader ).toList.sortBy(_._2)(viceOrdering) match {
      case (_,firstCards) :: ( _, secondCards) :: _ if viceOrdering.equiv(firstCards,secondCards) =>  None 
      case (player,_) :: _ => Some(player)
    }
    case _ => None
  }
  def totalBids: Map[Player,List[Card]] = totalsOrderedByLastPlayed.toMap
}
case class TwoTrumps(chief:Player,chiefTrump:Trump,partner:Player,viceTrump:Trump) extends ResolvedAuction
case class OneTrump(chief:Player,chiefTrump:Trump,partner:Player) extends ResolvedAuction
case class ThreePlayerFinish(chief:Player,chiefTrump:Trump) extends ResolvedAuction

case class ChiefAndVice( chief:Player,vice:Player) extends FinishedAuction
case class ChiefOnly( chief:Player) extends FinishedAuction
case class ChiefThreePlayers(chief:Player) extends FinishedAuction
case class Eklat(lastPlayed: Player, othersOnEqual: Set[Player]) extends FinishedAuction with ResolvedAuction 
case class EklatNoPoints(players:Vector[Player],bids:Vector[Bid]) extends FinishedAuction with ResolvedAuction


sealed trait AuctionObject {
  val players: Vector[Player]
  val bids: Vector[Bid]
}

case class UnfinishedAuction(status: AuctionStatus) extends AuctionObject {
  import Auction._
  def bid(b:Bid):AuctionObject = {
    val newBids = bids:+b
    val newStatus = status.copy(bids = newBids)
    if (!newStatus.isFinished) UnfinishedAuction(newStatus)
    else if (newStatus.allArePasses) FinishedAuction(newStatus,EklatNoPoints)
    else newStatus.leadersOrderedByLastPlayed match {
      case List(chief) if playerCount == 3 => FinishedAuction(ChiefThreePlayers(players,newBids,chief))
      case List(chief)  => newStatus.currentVice match {
        case Some(vice) => ChiefAndVice(players,newBids,chief,vice)
        case None => ChiefOnly(players,newBids,chief)
      }
      case List(offender,others@_*) => Eklat(players,newBids,offender,others.toSet)
    }
  }

}
