package com.joecordingley.mu

import org.scalatest.{FreeSpec, Matchers}
import cats._
import cats.effect.IO
import AuctionPlay._
import cats.implicits._
import cats.data.State
import com.joecordingley.mu.Auction._

class AuctionTest extends FreeSpec with Matchers {

  val fivePlayers = (1 to 5).toList.map(Player(_))
  val playerHands: List[(Player, InitialHand)] =
    fivePlayers.map(_ -> Set.empty[Card]).toList
  val redNine = Card(Red, OtherRank(9))
  val blueEight = Card(Blue, OtherRank(8))
  val yellow7 = Card(Yellow, FirstSeven)
  val purple4 = Card(Purple, OtherRank(4))
  val green9 = Card(Green, OtherRank(9))
  val red9 = Set(redNine)
  val twoCards = Set(blueEight, yellow7)
  val twoCardRaise = Raise(twoCards)
  val raise = Raise(red9)
  val lowerRaise = Raise(Set(purple4))
  val equalRaise = Raise(Set(green9))
  val initial = Auction.initial(playerHands)
  val fivePasses = List.fill(5)(Pass)
  val fourPasses = List.fill(4)(Pass)

  type BidState[A] = State[AuctionObject, A]

  val bidRoundStateCompiler = new (AuctionADT ~> BidState) {
    def apply[A](fa: AuctionADT[A]): BidState[A] = fa match {
      case PlaceBid(b) =>
        State.modify {
          case s: UnfinishedAuction => s.bid(b)
          case _                    => throw new Exception("is finished")
        }
      case _ => throw new Exception("unexpected input")
    }
  }

  def placeBidsOnInitial(bids: List[Bid]) =
    placeBids(bids)
      .foldMap(bidRoundStateCompiler)
      .run(initial)
      .value
      ._1

  def placeBids(bids: List[Bid]): AuctionFree[Unit] =
    bids
      .traverse(AuctionPlay.placeBid)
      .map(_ => ())

  "auction" - {
    "for 5 players" - {
      "when it isn't finished yet" - {
        "should return unfinished" in {
          val bids = twoCardRaise :: raise :: fourPasses
          val actual = placeBidsOnInitial(bids)
          actual shouldBe an[UnfinishedAuction]
        }
      }
      "when one definite winner" - {
        "and one definite vice" - {
          "should pick the correct chief and vice" in {
            val chief = fivePlayers(0)
            val vice = fivePlayers(1)
            val bids = twoCardRaise :: raise :: fivePasses
            val actual = placeBidsOnInitial(bids)
            actual shouldBe a[FinishedAuctionObject]
            actual
              .asInstanceOf[FinishedAuctionObject]
              .outcome shouldEqual ChiefAndVice(chief, vice)
            actual.state.validPartners shouldEqual fivePlayers.toSet - chief - vice
          }
        }
        "and a tiebreak vice" - {
          "should pick the correct chief and vice" in {
            val chief = fivePlayers(0)
            val vice = fivePlayers(1)
            val bids = twoCardRaise :: raise :: lowerRaise :: fivePasses
            val actual = placeBidsOnInitial(bids)
            actual shouldBe a[FinishedAuctionObject]
            actual
              .asInstanceOf[FinishedAuctionObject]
              .outcome shouldEqual ChiefAndVice(chief, vice)
            actual.state.validPartners shouldEqual fivePlayers.toSet - chief - vice
          }
        }
        "but no definite vice" - {
          "should pick the correct chief but no vice" in {
            val chief = fivePlayers(0)
            val bids = twoCardRaise :: raise :: equalRaise :: fivePasses
            val actual = placeBidsOnInitial(bids)
            actual shouldBe a[FinishedAuctionObject]
            actual
              .asInstanceOf[FinishedAuctionObject]
              .outcome shouldEqual ChiefOnly(chief)
            actual.state.validPartners shouldEqual fivePlayers.toSet - chief
          }
        }
      }
      "with five passes" - {
        "should return Eklat with no points" in {
          val bids = fivePasses
          val actual = placeBidsOnInitial(bids)
          actual shouldBe a[FinishedAuctionObject]
          actual
            .asInstanceOf[FinishedAuctionObject]
            .outcome shouldEqual EklatNoPoints
        }
      }
      "with no definite winner" - {
        "should return Eklat with the offending player and the offended player" in {
          val bids = lowerRaise :: raise :: fivePasses
          val offender = fivePlayers(1)
          val offendeds = Set[Player](fivePlayers(0))
          val actual = placeBidsOnInitial(bids)
          actual shouldBe a[FinishedAuctionObject]
          actual.asInstanceOf[FinishedAuctionObject].outcome shouldEqual Eklat(
            offender,
            offendeds)
        }
      }
    }
  }
}
