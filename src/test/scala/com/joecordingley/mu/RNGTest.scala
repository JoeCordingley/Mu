package com.joecordingley.mu

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}


class RNGTest extends FreeSpec with Matchers with Checkers with GeneratorDrivenPropertyChecks {

  "An RNG with the same seed should return the same next random number" in {
    forAll{ (n:Long) => 
      Random.nextLong(n) shouldEqual Random.nextLong(n) 
    }
  }
}
