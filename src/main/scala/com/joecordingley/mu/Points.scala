package com.joecordingley.mu

import com.joecordingley.mu.Game.Scores
import monocle.Lens

/**
  * Created by joe on 01/07/17.
  */
object Points {

  def goal(numberOfPlayers:Int,cardsBid:Int):Int = numberOfPlayers match {
    case 3 => 10 + cardsBid*2
    case 4 => 28 + cardsBid*2
    case 5 => 21 + cardsBid*3
    case 6 => 16 + cardsBid*4
  }

  def bonusAmount(chiefTrump:Trump,cardsBid:Int):Int = chiefTrump match {
    case _:SuitTrump => cardsBid*10
    case NumberTrump(1)|NumberTrump(7) =>10 + cardsBid*10
    case NumberTrump(_) => 20 + cardsBid*10
    case NoTrump => 30 + cardsBid*10
  }

  sealed trait GoalOutcome
  case object GoalMet extends GoalOutcome
  case class GoalMissed(margin:Int) extends GoalOutcome

  def getGoalOutcome(numberOfPlayers:Int,cardsBid:Int,scoreGot:Int):GoalOutcome ={

    def scoreMet(cardsBid:Int):Boolean = scoreGot > goal(numberOfPlayers,cardsBid)
    def missedBy(cardsBid:Int,acc:Int):Int = if(scoreMet(cardsBid)) acc else missedBy(cardsBid-1,acc+1)

    if (scoreMet(cardsBid)) GoalMet
    else GoalMissed(missedBy(cardsBid,0))
  }

  def getBonusesAndPenalties(scores:Map[Player,Int],chief:Player,partner:Player,chiefTrump:Trump,cardsBid:Int):Map[Player,Int] = {
    val numberOfPlayers = scores.size
    val chiefsTeamScore = scores(chief) + scores(partner)
    val goalOutcome = getGoalOutcome(numberOfPlayers,cardsBid,chiefsTeamScore)
    goalOutcome match {
      case GoalMet => {
        val chiefTeamsBonus = bonusAmount(chiefTrump,cardsBid)
        Map(chief -> chiefTeamsBonus,partner -> chiefTeamsBonus)
      }
      case GoalMissed(margin) => {
        val chiefsPenalty = margin*10
        val oppositionBonus = margin*5
        val players = scores.keys
        val opposition = players.filter(player => player!=chief && player!=partner)
        Map(chief -> -chiefsPenalty)++opposition.map(_->oppositionBonus)
      }
    }

  }

 // def addBonusesAndPenalties(scores:Map[Player,Int],chief:Player,partner:Player,chiefTrump:Trump,cardsBid:Int):Map[Player,Int] = {
 //   import Util._
 //   val bonuses = getBonusesAndPenalties(scores,chief,partner,chiefTrump,cardsBid)
 //   mapKeys[Player,Int](bonuses.keys.toSet).modify{case (player,score) => (player,score + bonuses(player))}(scores)

 // }

}
