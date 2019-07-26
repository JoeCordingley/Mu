package com.joecordingley.mu

import cats.~>

object AvailableMoves {
  type Request[A] = (AuctionADT[A], AuctionState)
  def auction[A](action: AuctionADT[A], state: AuctionState): Set[A] = (action, state) match {
    case (GetBid(player),AuctionState(playerHands,_,_)) => playerHands.toMap.apply(player).subsets.map{ set =>
      if (set.isEmpty) Pass else Raise(set)
    }.toSet[Bid]
  }
}
