package com.joecordingley.mu

import com.joecordingley.mu.CardPlayObject._

case class CardPlayObject(playerHands: List[(Player, Set[Card])],
                          trumps: Trumps,
                          tricks: Vector[TrickAsPlayed],
                          currentTrick: Vector[Card],
                          numberOfCardsBid: Int,
                          chief: Player,
                          partner: Option[Player]) {
  val players = playerHands.map(_._1)

  private def cardsPreviouslyPlayedBy(player: Player): Set[Card] = for {
    trick: TrickAsPlayed <- tricks.toSet
    playedCard <- trick
    if playedCard.player == player
  } yield playedCard.card

  def cardsNotYetInFinishedTrick(player: Player):Set[Card] = playerHands.toMap.apply(player) &~ cardsPreviouslyPlayedBy(player)

  def playCard(card: Card): CardPlayObject =
    this.copy(currentTrick = currentTrick :+ card)
  def recordTrick(wonTrick: TrickAsPlayed) =
    this.copy(currentTrick = Vector.empty, tricks = tricks :+ wonTrick)

  def getPlayersStartingFrom(player: Player): List[Player] = {
    val (later, earlier) = players.span(_ != player)
    earlier ::: later
  }

  def getWinner(trick: List[PlayedCard]): Player = {
    val cardLed = trick.head.card
    val cardOrdering = CardOrdering(trumps, cardLed)
    trick.sortBy(_.card)(cardOrdering).head.player
  }

  def getScores(wonTricks: List[WonTrick]): CardPlayScores =
    Points.getTotalScores(wonTricks,
                          trumps.major,
                          numberOfCardsBid,
                          chief,
                          partner,
                          players)

}

object CardPlayObject {
  type TrickAsPlayed = List[PlayedCard]
  type Trick = List[Card]
  type CardPlayScores = Map[Player, Int]
}
