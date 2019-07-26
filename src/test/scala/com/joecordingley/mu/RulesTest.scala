package com.joecordingley.mu

import org.scalatest.{FreeSpec, Matchers}
import com.joecordingley.mu.Auction.InitialHand

class RulesTest extends FreeSpec with Matchers {

  val fivePlayers = (1 to 5).toList.map(Player)
  val playerOne = Player(1)
  val playerTwo = Player(2)

  implicit def intToRank(i: Int): Rank = {
    val validRanks = Set(0,2,3,4,5,6,8,9)
    if (validRanks contains i) OtherRank(i) else fail
  }

  val redFive = Card(Red, 5)
  val blueSix = Card(Blue, 6)

  
  "AvailableMoves auction" - {
    val playerOnesHand = Set(redFive)
    val playerTwosHand = Set(blueSix)
    val playerHands = List(
      playerOne -> playerOnesHand,
      playerTwo -> playerTwosHand
    )
    val auctionState = AuctionState(playerHands= playerHands)
    val availableMoves = AvailableMoves.auction(GetBid(playerOne), auctionState)
    "should contain Pass" in {
      availableMoves should contain (Pass)
    }
    "should only allow bids that include players cards in hand" in {
      val allCards = availableMoves.flatMap{
        case Raise(cards) => cards
        case Pass => Set.empty[Card]
      }
      allCards should contain (redFive)
    }
    "should not allow bids with cards that have "
  }

}
