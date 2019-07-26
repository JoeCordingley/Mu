package com.joecordingley.mu

import com.joecordingley.mu.Game.Scores
import monocle.Lens

/**
  * Created by joe on 01/07/17.
  */
object Points {

  def goal(numberOfPlayers: Int, cardsBid: Int): Int = numberOfPlayers match {
    case 3 => 10 + cardsBid * 2
    case 4 => 28 + cardsBid * 2
    case 5 => 21 + cardsBid * 3
    case 6 => 16 + cardsBid * 4
  }

  def bonusAmount(chiefTrump: Trump, cardsBid: Int): Int = chiefTrump match {
    case _: SuitTrump => cardsBid * 10
    case NumberTrump(1) | NumberTrump(7) => 10 + cardsBid * 10
    case NumberTrump(_) => 20 + cardsBid * 10
    case NoTrump => 30 + cardsBid * 10
  }

  sealed trait GoalOutcome
  case object GoalMet extends GoalOutcome
  case class GoalMissed(margin: Int) extends GoalOutcome

  def getGoalOutcome(
      numberOfPlayers: Int,
      cardsBid: Int,
      scoreGot: Int): GoalOutcome = {

    def scoreMet(cardsBid: Int): Boolean =
      scoreGot > goal(numberOfPlayers, cardsBid)
    def missedBy(cardsBid: Int): Int =
      cardsBid - ((cardsBid to 1 by -1) find scoreMet getOrElse 0)

    if (scoreMet(cardsBid)) GoalMet
    else GoalMissed(missedBy(cardsBid))
  }

  def getBonusesAndPenalties(
      scores: Map[Player, Int],
      chief: Player,
      partner: Option[Player],
      chiefTrump: Trump,
      cardsBid: Int): Map[Player, Int] = {
    val players = scores.keys.toSet
    val numberOfPlayers = players.size
    val chiefsTeamScore = scores(chief) + (partner map scores getOrElse 0)
    val goalOutcome = getGoalOutcome(numberOfPlayers, cardsBid, chiefsTeamScore)
    goalOutcome match {
      case GoalMet => {
        val chiefTeamsBonus = bonusAmount(chiefTrump, cardsBid)
        Map(chief -> chiefTeamsBonus) ++ partner.map(_ -> chiefTeamsBonus)
      }
      case GoalMissed(margin) => {
        val chiefsPenalty = margin * 10
        val oppositionBonus = margin * 5
        val opposition = players - chief -- partner
        Map(chief -> -chiefsPenalty) ++ opposition.map(_ -> oppositionBonus)
      }
    }

  }

  def getScoresWithoutBonuses(
      players: List[Player],
      tricks: List[WonTrick]): Map[Player, Int] = {
    val scores = tricks.groupBy(_.winner).mapValues { wonTricks =>
      val cardPoints = for {
        wonTrick <- wonTricks
        card <- wonTrick.trick
      } yield card.points
      cardPoints.sum
    }
    scores ++ players.filterNot(scores.keys.toSet).map(_ -> 0)
  }

  def getTotalScores(
      tricks: List[WonTrick],
      chiefTrump: Trump,
      cardsBid: Int,
      chief: Player,
      partner: Option[Player],
      players: List[Player]) = {
    val scores = getScoresWithoutBonuses(players, tricks)
    val bonusesAndPenalties =
      getBonusesAndPenalties(scores, chief, partner, chiefTrump, cardsBid)
    bonusesAndPenalties.foldLeft(scores) {
      case (acc, (player, bonus)) => acc + (player -> (acc(player) + bonus))
    }
  }

}
