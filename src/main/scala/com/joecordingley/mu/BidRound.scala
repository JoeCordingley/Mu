package com.joecordingley.mu

import cats.data.StateT
import cats.effect.IO
//import State._
//import monocle._
//import monocle.macros.GenLens
//import Util._
//
///**
//  * Created by joe on 23/06/17.
//  */
case class BidRoundState()
object BidRound {

  type BidRoundStateIO[A] = StateT[IO,BidRoundState,A]


}

//object BidRound {
//  def isFinished(bidState: BidState):Boolean = bidState.passes == bidState.players.all.size
//
//  def go : State[BidState,FinishedBids] = for {
//    bidState <- get[BidState]
//    finishedBids <- if (isFinished(bidState)) pure[BidState,FinishedBids](FinishedBids(bidState.bids)) else for {
//      player <- currentPlayer
//      bid <- getBid(player)
//      bidEvent = BidEvent(player,bid)
//      _ <- updateState(bidEvent)
//      _ <- updatePlayerState(bidEvent)
//      _ <- nextPlayer
//      finishedBids <- go
//    } yield finishedBids
//  } yield finishedBids
//
//
//  implicit val playersLens: Lens[BidState, Players] = GenLens[BidState](_.players)
//  val listLens: Lens[Players,List[Player]] = GenLens[Players](_.all)
//  val eachPlayer: Traversal[BidState, Player] = playersLens composeLens listLens composeTraversal Players.eachPlayer
//  type Bids = Map[Player,TotalBid]
//  val bidsLens: Lens[BidState,Bids] = GenLens[BidState](_.bids)
//  val passesLens: Lens[BidState,Int] = GenLens[BidState](_.passes)
//
//  import StateHelper.lift
//  def updateState(event: BidEvent):State[BidState,Unit] = modify(updateBidState(event))
//  def getBid(player:Player):State[BidState,Bid] = pure(player.getBid)
//  def nextPlayer:State[BidState,Player] = Players.nextPlayer
//  def currentPlayer:State[BidState,Player] = Players.currentPlayer
//  def updatePlayerState(event: BidEvent):State[BidState,Unit] = modify(updatePlayers(event))
//  def updatePlayers(event: BidEvent): (BidState) => BidState = eachPlayer.modify(_.update(event))
//  def updateBidState(event: BidEvent): (BidState) => BidState = event match {
//    case BidEvent(_,Pass) => passesLens.modify(_+1)
//    case BidEvent(player,raise:Raise) => passesLens.modify(_=>0) andThen
//      bidLens(player).modify(raiseBid(raise))
//  }
//  def playerMapLens(player: Player): Lens[Bids, TotalBid] = mapLens[Player,TotalBid](player)
//  def bidLens(player: Player): Lens[BidState,TotalBid] = bidsLens composeLens playerMapLens(player)
//  def raiseBid(raise: Raise)(originalBid: TotalBid):TotalBid = TotalBid(originalBid.cards:::raise.cards)
//
//  def addBid(additionalBid:Raise)(totalBid: TotalBid) = TotalBid(totalBid.cards:::additionalBid.cards)
//
//
//}
//
//case class BidEvent(player:Player, bid: Bid) extends Event
//
//object TotalBid {
//  def empty = new TotalBid(Nil)
//}
//
//sealed trait Bid
//case object Pass extends Bid
//case class Raise(cards:List[Card]) extends Bid
//
//case class TotalBid(cards:List[Card]){
//  def size = cards.size
//
//}
//
//case class BidState(players:Players,bids:Map[Player,TotalBid],passes:Int){
//
//}
//case class FinishedBids(bids:Map[Player,TotalBid]){
//  val nobodyBid:Boolean = bids.values.forall(_.size ==0)
//  val isEklat:Boolean = {
//    val bidSizes = bids.mapValues(_.size)
//    val (_,maxBid) = bidSizes.maxBy{ case (player,amount) => amount}
//    val winners = bidSizes.filter{case (_,amount) => amount == maxBid}
//    winners.size > 1
//  }
//}
//
//
//object BidState {
//  def initial(players:Players):BidState  = {
//    val initialBids = players.all.map((_,TotalBid.empty)).toMap
//    BidState(players,initialBids,0)
//  }
//}
