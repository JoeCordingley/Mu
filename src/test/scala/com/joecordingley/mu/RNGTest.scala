package com.joecordingley.mu

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}

class RNGTest
    extends FreeSpec
    with Matchers
    with Checkers
    with GeneratorDrivenPropertyChecks {

  "An RNG with the same seed should return the same next random number" in {
    forAll { (n: Long) =>
      Random.nextLong(n) shouldEqual Random.nextLong(n)
    }
  }
  "shuffling elements should still have the same elements" in {
    forAll{ (n: Long, l: Vector[Int]) =>  
      RandomState.shuffle(l).runA(n).value should contain theSameElementsAs l
    }
  }
  "deal should deal out all the cards evenly" in {
    val numberOfPlayers = 5
    val fullDeck = Deck.Full
    forAll{ (n: Long) =>
      val playerHands = RandomState.dealAllEvenly(fullDeck, numberOfPlayers).runA(n).value 
      playerHands should have size 5
      playerHands.foreach(pile => pile should have size (fullDeck.size / numberOfPlayers))
    } 
  }

}
