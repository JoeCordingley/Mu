package com.joecordingley.mu


sealed trait Bid
case object Pass extends Bid
case class Raise(cards:List[Card]) extends Bid

object Auction {
  def initial(players:List[Player]):Auction = new Auction(players,Nil)

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

sealed trait AuctionStatus

case object Unfinished extends AuctionStatus 
case class Finished(finishedBidRound: BidRoundOutcome) extends AuctionStatus

sealed trait BidRoundOutcome
sealed trait FinishedAuctionRound
case class TwoTrumps(chief:Player,chiefTrump:Trump,partner:Player,viceTrump:Trump) extends FinishedAuctionRound
case class OneTrump(chief:Player,chiefTrump:Trump,partner:Player) extends FinishedAuctionRound
case class ThreePlayerFinish(chief:Player,chiefTrump:Trump) extends FinishedAuctionRound

case class ChiefAndVice(chief:Player,vice:Player) extends BidRoundOutcome
case class ChiefOnly(chief:Player) extends BidRoundOutcome
case class ChiefThreePlayers(chief:Player) extends BidRoundOutcome
case class Eklat(lastPlayed:Player,othersOnEqual:Set[Player]) extends BidRoundOutcome with FinishedAuctionRound
case object EklatNoPoints extends BidRoundOutcome with FinishedAuctionRound


case class Auction(players:List[Player],bidsReversed:List[Bid]){
  import Auction._
  val bids = bidsReversed.reverse
  val playerCount = players.length
  val totalActions: Int = bidsReversed.length
  val currentPlayer: Player = players(totalActions%playerCount)
  def bid(b:Bid):Auction = this.copy(bidsReversed = b :: bidsReversed)
  val playerSequence = Stream.continually(players).flatten
  val playerBids = playerSequence.zip(bids).toList
  val totalsOrderedByLastPlayed = {
    val empties: List[(Player,List[Card])] = players.map(_->List[Card]())
    playerBids.foldLeft(empties){
      case (totals,(player,Raise(cards))) => {
        val total = totals.toMap.apply(player)
        (player -> (total ::: cards)) :: totals.filterNot(_._1 == player)
      }
      case (totals,_) => totals
    }
  }
  val leadersOrderedByLastPlayed = {
    val maxBid: Int = totalsOrderedByLastPlayed.map{
      case (_,cards) => cards.size
    }.max
    totalsOrderedByLastPlayed.collect{
      case (player,cards) if cards.size == maxBid => player
    }
  }
  val totalBids: Map[Player,List[Card]] = totalsOrderedByLastPlayed.toMap
  lazy val currentVice:Option[Player] = leadersOrderedByLastPlayed match {
    case List(leader) => ( totalBids - leader ).toList.sortBy(_._2)(viceOrdering) match {
      case (_,firstCards) :: ( _, secondCards) :: _ if viceOrdering.equiv(firstCards,secondCards) =>  None 
      case (player,_) :: _ => Some(player)
    }
    case _ => None
  }

  lazy val status: AuctionStatus = {
    def go(bids: List[Bid], passes:Int):AuctionStatus = bids match {
      case _ if passes == playerCount => Finished(finish)
      case Nil => Unfinished
      case (_:Raise) :: _ => Unfinished
      case Pass :: rest => go(rest, passes + 1)
    }
    def allArePasses:Boolean = bidsReversed.forall( _.isInstanceOf[Pass.type] )
    def finish:BidRoundOutcome = 
      if (allArePasses) EklatNoPoints 
      else leadersOrderedByLastPlayed match {
        case List(leader) if playerCount == 3 => ChiefThreePlayers(leader)
        case List(leader)  => currentVice match {
          case Some(vice) => ChiefAndVice(leader,vice)
          case None => ChiefOnly(leader)
        }
        case List(offender,others@_*) => Eklat(offender,others.toSet)
      }
    go(bidsReversed,0)
  }
}
