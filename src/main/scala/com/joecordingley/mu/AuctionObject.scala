package com.joecordingley.mu
import Auction._
import com.joecordingley.mu.Util._

sealed trait Bid
case object Pass extends Bid
case class Raise(cards: Set[Card]) extends Bid

object Auction {
  def initial(playerHands: List[(Player, InitialHand)]): UnfinishedAuction =
    new UnfinishedAuction(AuctionState(playerHands, Vector()))

  val viceOrdering: Ordering[Set[Card]] = {
    def amtForRank(rank: Int, cards: Set[Card]) =
      cards.filter(_.rank.value == rank).size
    def amtForAllRanks(cards: Set[Card]) =
      (9 to 0 by -1).map(rank => amtForRank(rank, cards)).toList
    Ordering.by[Set[Card], Iterable[Int]](cards =>
      cards.size :: amtForAllRanks(cards))
  }
  type InitialHand = Set[Card]
}

sealed trait ResolvedAuction
sealed trait AuctionOutcome
case class FinishedAuctionObject(state: AuctionState, outcome: AuctionOutcome)
    extends AuctionObject
case class AuctionState(playerHands: List[(Player, InitialHand)],
                        bids: Vector[Bid]) {
  val players = playerHands.map(_._1)
  val playerCount = players.size
  val playerSequence = Stream.continually(players).flatten
  val playerBids = playerSequence.zip(bids).toList
  val totals = {
    val empties: Map[Player, Set[Card]] =
      players.map(_ -> Set.empty[Card]).toMap
    playerBids.foldLeft(empties) {
      case (acc, (player, Raise(cards))) =>
        acc.updated(player, acc(player) ++ cards)
      case (acc, _) => acc
    }
  }
  val cardsInHand: Map[Player, Set[Card]] = totals.foldLeft(playerHands.toMap) {
    case (acc, (player, bidTotal)) =>
      acc.updated(player, acc(player) &~ bidTotal)
  }
  val maxBid: Int = totals.values.map(_.size).max
  val leaders: Set[Player] = totals.collect {
    case (player, bid) if bid.size == maxBid => player
  }.toSet
  val lastOfLeadersToRaise: Option[Player] = playerBids.reverse.collectFirst {
    case (player, _: Raise) if leaders(player) => player
  }

  def isFinished: Boolean =
    if (bids.size < playerCount) false
    else bids.reverse.take(playerCount).forall(_.isInstanceOf[Pass.type])

  val vice: Option[Player] = leaders match {
    case SingleElementSet(leader) =>
      (totals - leader).toList.outrightHighest(viceOrdering.on(_._2)).map(_._1)
    case _ => None
  }

  val currentPlayer: Player = players(bids.size % playerCount)
}
case class TwoTrumps(chief: Player,
                     chiefTrump: Trump,
                     partner: Player,
                     viceTrump: Trump)
    extends ResolvedAuction
case class OneTrump(chief: Player, chiefTrump: Trump, partner: Player)
    extends ResolvedAuction
case class ThreePlayerFinish(chief: Player, chiefTrump: Trump)
    extends ResolvedAuction

case class ChiefAndVice(chief: Player, vice: Player) extends AuctionOutcome
case class ChiefOnly(chief: Player) extends AuctionOutcome
case class ChiefThreePlayers(chief: Player) extends AuctionOutcome
case class Eklat(lastPlayed: Player, othersOnEqual: Set[Player])
    extends AuctionOutcome
    with ResolvedAuction
case object EklatNoPoints extends AuctionOutcome with ResolvedAuction

sealed trait AuctionObject {
  val state: AuctionState
}

case class UnfinishedAuction(state: AuctionState) extends AuctionObject {
  def bid(b: Bid): AuctionObject = {
    val newBids = state.bids :+ b
    val newStatus = state.copy(bids = newBids)
    if (!newStatus.isFinished) UnfinishedAuction(newStatus)
    else
      newStatus.leaders match {
        case SingleElementSet(chief) if state.playerCount == 3 =>
          FinishedAuctionObject(newStatus, ChiefThreePlayers(chief))
        case SingleElementSet(chief) =>
          newStatus.vice match {
            case Some(vice) =>
              FinishedAuctionObject(newStatus, ChiefAndVice(chief, vice))
            case None => FinishedAuctionObject(newStatus, ChiefOnly(chief))
          }
        case leaders =>
          newStatus.lastOfLeadersToRaise match {
            case Some(offender) =>
              FinishedAuctionObject(newStatus,
                                    Eklat(offender, leaders - offender))
            case None => FinishedAuctionObject(newStatus, EklatNoPoints)
          }
      }
  }

}
