package com.joecordingley.mu
//
//import cats.data.State
//import State._
//import monocle.Traversal
//import scalaz.std.list._   // to get the Traverse instance for List
//
///**
//  * Created by joe on 24/06/17.
//  */
//object Players {
//  def makeThisPlayerFirst(firstPlayer: Player)(players: List[Player]) = {
//    val (laterPlayers,firstPlayers) = players.span(_!=firstPlayer)
//    firstPlayers ::: laterPlayers
//  }
//
//
//  def nextPlayer: State[Players,Player] = State[Players,Player](players => (players.nextPlayers,players.current))
//  def currentPlayer: State[Players,Player] = get[Players].map(_.current)
//  def getBid: State[Player,Bid] = get[Player].map(_.getBid)
//  def eachPlayer = Traversal.fromTraverse[List,Player]
//
//}
//case class Players(all:List[Player]) {
//  val current:Player = all.head
//  def nextPlayers:Players = Players(all.tail :+ current)
//}
//trait Event
trait Player{


}
