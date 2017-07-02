/*package com.joecordingley.mu

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by joe on 23/06/17.
  */
class BidRoundTest extends FlatSpec with Matchers{

  "nextPlayer" should "return the current player and the state with the new player" in {
    val player1 = DummyPlayer(1)
    val player2 = DummyPlayer(2)
    val player3 = DummyPlayer(3)
    val players = Players(List(
      player1,
      player2,
      player3
    ))
    val bidState = BidState.initial(players)
    val (bidstate,player) = BidRound.nextPlayer.run(bidState).value
    player should equal(player1)
    bidstate.players.all should equal(List(player2,player3,player1))

  }

}*/
