package com.joecordingley.mu

import org.scalatest.{FreeSpec,Matchers}
import cats._
import cats.effect.IO
import AuctionPlay._
import cats.implicits._
import cats.data.State

class AuctionTest extends FreeSpec with Matchers {

  def dummyPlayer  = new Player {}
  case class DummyPlayer(i:Int) extends Player
  val fivePlayers = (1 to 5).toList.map(DummyPlayer(_))
  val redNine = Card(Red,9)
  val blueEight = Card(Blue,8)
  val yellow7 = Card(Yellow,7)
  val purple4 = Card(Purple,4)
  val green9 = Card(Green,9)
  val red9 = List(redNine)
  val twoCards = List(blueEight,yellow7)
  val twoCardRaise = Raise(twoCards)
  val raise = Raise(red9)
  val lowerRaise = Raise(List(purple4))
  val equalRaise = Raise(List(green9))
  val initial = Auction.initial(fivePlayers)
  val fivePasses = List.fill(5)(Pass)
  val fourPasses = List.fill(4)(Pass)

  type BidState[A] = State[AuctionObject, A]
  val bidRoundStateCompiler = new (AuctionADT ~> BidState){
    def apply[A](fa: AuctionADT[A]):BidState[A] = fa match { 
      case PlaceBid(b) => State.modify(_.bid(b))
      case _ => throw new Exception("unexpected input")
    }
  }

  def placeBidsRunInitial(bids:List[Bid]) = placeBids(bids)
    .foldMap(bidRoundStateCompiler)
    .run(initial)
    .value

  def placeBids(bids: List[Bid]):AuctionFree[Unit] = bids.traverse(AuctionPlay.placeBid)
    .map(_ =>())

  "auction" - {
    "for 5 players" - {
      "when it isn't finished yet" - {
        "should return unfinished" in {
          val bids = twoCardRaise :: raise :: fourPasses
          val actual = placeBidsRunInitial(bids)._1.status
          actual should equal (UnfinishedAuction)
        }
      }
      "when one definite winner" -{
        "and one definite vice" -{
          "should pick the correct chief and vice" in {
            val chief = fivePlayers(0)
            val vice = fivePlayers(1)
            val bids = twoCardRaise :: raise :: fivePasses
            val outcome = ChiefAndVice(chief,vice)
            val expected = Finished(outcome)
            val actual = placeBidsRunInitial(bids)._1.status
            actual should equal (expected)
          }
        }
        "and a tiebreak vice" - {
          "should pick the correct chief and vice" in {
            val chief = fivePlayers(0)
            val vice = fivePlayers(1)
            val bids = twoCardRaise :: raise :: lowerRaise :: fivePasses
            val outcome = ChiefAndVice(chief,vice)
            val expected = Finished(outcome)
            val actual = placeBidsRunInitial(bids)._1.status
            actual should equal (expected)
          }
        }
        "but no definite vice" - {
          "should pick the correct chief but no vice" in {
            val chief = fivePlayers(0)
            val bids = twoCardRaise :: raise :: equalRaise :: fivePasses
            val outcome = ChiefOnly(chief)
            val expected = Finished(outcome)
            val actual = placeBidsRunInitial(bids)._1.status
            actual should equal (expected)
          }
        }
      }
      "with five passes" -{
        "should return Eklat with no points" in {
          val bids = fivePasses
          val outcome = EklatNoPoints
          val expected = Finished(outcome)
          val actual = placeBidsRunInitial(bids)._1.status
          actual should equal (expected)
        }
      }
      "with no definite winner" - {
        "should return Eklat with the offending player and the offended player" in {
          val bids = lowerRaise :: raise :: fivePasses
          val offender = fivePlayers(1)
          val offendeds = Set[Player](fivePlayers(0))
          val expected = Finished(Eklat(offender,offendeds))
          val actual = placeBidsRunInitial(bids)._1.status
          actual should equal (expected)
        }
      }
    }
  }
  "totalBids" - {
    "should return the total bids" in {
      val totalBids = placeBidsRunInitial(List(Pass,raise,Pass,Pass,Pass,Pass,twoCardRaise))._1.totalBids
      totalBids(DummyPlayer(2)) should contain theSameElementsAs (red9:::twoCards)
      for ( i <- List(1,3,4,5)){
        totalBids(DummyPlayer(i)) shouldBe empty
      }

    }
  }
  "winningPlayers" - {
    "should return the winningPlayers" in {
      val winners = placeBidsRunInitial(List(Pass,raise,Pass,Pass,Pass,Pass,twoCardRaise))._1.leadersOrderedByLastPlayed
      winners should equal ( List(DummyPlayer(2)) )
    }
  }
  "initial bid state" - {
    "should return a bid state with all players and no events" in {
      initial.players should be ( fivePlayers )
      initial.bids should equal ( Nil )
    }
  }
  "bid" - {
    "should add a bidEvent to the bid state" in {
      initial.bid(Pass).bid(raise).bids should equal ( List(Pass,raise) )
    }
  }

  "placeBids" - {
    "should place bids" in {
      val expected = List(Pass,raise)
      val actual = placeBidsRunInitial(List(Pass,raise))._1
        .bids
      actual should equal ( expected )
    }
  }

//
//  "determineOutcome" - {
//    "should return EklatNoPoints when nobody has played" in {
//      val expected = EklatNoPoints
//      val actual = BidRoundOutcome(fivePassesState)
//      actual shouldBe (expected)
//    }
//    "should return Eklat with offending player when two played top amount " in {
//      object OffendingPlayer extends Player
//      object OtherPlayer extends Player
//      val expected = Eklat(OffendingPlayer)
//      val players =  dummyPlayer :: dummyPlayer :: dummyPlayer :: OtherPlayer :: OffendingPlayer :: Nil
//      val otherPlayerRaise = BidEvent(OtherPlayer,raise)
//      val offendingPlayerRaise = BidEvent(OffendingPlayer,raise)
//      val allPass = players.map(BidEvent(_,Pass))
//      val bidEvents = allPass ::: offendingPlayerRaise :: otherPlayerRaise:: Nil
//      val bidRoundState = BidRoundState(players,bidEvents)
//      val actual = BidRoundOutcome(bidRoundState)
//      actual should equal (expected)
//    }
//    "should return the chief and vice in a normal bidding round" in {
//
//
//    }
//  }
  
}
