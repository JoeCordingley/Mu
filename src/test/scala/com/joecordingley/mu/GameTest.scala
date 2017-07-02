/*package com.joecordingley.mu

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by joe on 23/06/17.
  */
class GameTest extends FlatSpec with Matchers{

  "nextPlayer State" should "return the current player and rotate" in {
    val player1 = DummyPlayer(1)
    val player2 = DummyPlayer(2)
    val player3 = DummyPlayer(3)
    val players = Players(List(player1,player2,player3))
    val (playersN,player) = Players.nextPlayer.run(players).value

    player should be(player1)
    playersN should be(Players(List(player2,player3,player1)))
  }
  "Player next" should "rotate the players" in {
    val player1 = DummyPlayer(1)
    val player2 = DummyPlayer(2)
    val player3 = DummyPlayer(3)
    val players = Players(List(player1,player2,player3))
    players.nextPlayers should be(Players(List(player2,player3,player1)))

  }

}
case class DummyPlayer(id:Int) extends Player {
  override def getBid: Bid = ???

  override def getCard: Card = ???

  override def update(bidEvent: Event): Player = ???
}*/
