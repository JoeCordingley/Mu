package com.joecordingley.mu

object CardOrdering {

  def cardIsOfLedSuit(cardLed: Card)(thisCard: Card): Boolean =
    cardLed.suit == thisCard.suit
  def cardIsDoubleTrump(trumps: Trumps)(card: Card): Boolean =
    cardIsMajorTrump(trumps)(card) && cardIsMinorTrump(trumps)(card)
  def cardIsMajorTrump(trumps: Trumps): (Card) => Boolean =
    cardIsTrump(trumps.major)
  def cardIsMinorTrump(trumps: Trumps): (Card) => Boolean =
    cardIsTrump(trumps.minor)

  def cardIsTrump(trump: Trump)(card: Card): Boolean = trump match {
    case SuitTrump(suit) => card.suit == suit
    case NumberTrump(rank) => card.rank.value == rank
    case NoTrump => false
  }

  def apply(trumps: Trumps, cardLed: Card): Ordering[Card] = Ordering.by {
    card =>
      (
        cardIsMajorTrump(trumps)(card),
        cardIsMinorTrump(trumps)(card),
        cardIsOfLedSuit(cardLed)(card),
        card.rank.value
      )
  }
}
