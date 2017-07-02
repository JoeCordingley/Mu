package com.joecordingley.mu

/**
  * Created by joe on 20/05/17.
  */
sealed trait Suit

case object Red extends Suit
case object Blue extends Suit
case object Yellow extends Suit
case object Green extends Suit
case object Purple extends Suit

case class Card(suit: Suit,rank:Int){
  lazy val points:Int = rank match {
    case 6 => 2
    case 7 => 2
    case 1 => 0
    case 9 => 0
    case _ => 1
  }
}

object Deck {

  val Suits: Set[Suit] = Set(Red,Blue,Yellow,Green,Purple)
  val ReducedSuits: Set[Suit] = Set(Red,Blue,Yellow)

  val Ranks: Set[Int] = (0 to 9).toSet

  def rankComposition(rank:Int): Int = rank match {
    case 1 => 2
    case 7 => 2
//    case x if x < 0 || x > 9 => throw new UserError
    case _ => 1
  }

  val RanksWithRepeats: List[Int] = for {
    rank <- Ranks.toList
    _ <- 0 until rankComposition(rank)
  } yield rank

  val Full: List[Card] = makeDeck(Suits)

  val Reduced: List[Card] = makeDeck(ReducedSuits)

  private def makeDeck(suits:Set[Suit]) = for {
    suit <- suits.toList
    rank <- RanksWithRepeats
  } yield Card(suit,rank)

}
