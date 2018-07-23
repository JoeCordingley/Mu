package com.joecordingley.mu

/**
  * Created by joe on 20/05/17.
  */
sealed trait Suit
sealed trait Rank {
  val value: Int
}
sealed trait Seven extends Rank {
  override val value: Int = 7
}
sealed trait One extends Rank {
  override val value: Int = 1
}
case object FirstSeven extends Seven
case object SecondSeven extends Seven
case object FirstOne extends One
case object SecondOne extends One
case class OtherRank(value: Int) extends Rank

case object Red extends Suit
case object Blue extends Suit
case object Yellow extends Suit
case object Green extends Suit
case object Purple extends Suit

case class Card(suit: Suit, rank: Rank) {
  lazy val points: Int = rank.value match {
    case 6 => 2
    case 7 => 2
    case 1 => 0
    case 9 => 0
    case _ => 1
  }
}

object Deck {

  val Suits: Set[Suit] = Set(Red, Blue, Yellow, Green, Purple)
  val ReducedSuits: Set[Suit] = Set(Red, Blue, Yellow)

  val Ranks: Set[Rank] = (0 to 9).flatMap {
    case 1 => List(FirstOne, SecondOne)
    case 7 => List(FirstSeven, SecondSeven)
    case x => List(OtherRank(x))
  }.toSet

  def rankComposition(rank: Int): Int = rank match {
    case 1 => 2
    case 7 => 2
//    case x if x < 0 || x > 9 => throw new UserError
    case _ => 1
  }

  val Full: List[Card] = makeDeck(Suits)

  val Reduced: List[Card] = makeDeck(ReducedSuits)

  private def makeDeck(suits: Set[Suit]) =
    for {
      suit <- suits.toList
      rank <- Ranks
    } yield Card(suit, rank)

}
