package com.joecordingley.mu

object CLIMain extends App {

  val players = (1 to 5).map(Player(_)).toList
  val dealt = Game.deal(players).runA(1l).value
  val initialAuctionObject = Auction.initial(dealt)
  val result = AuctionPlay.play
    .foldMap(AuctionCompilers.FullCompiler)
    .runA(initialAuctionObject)
    .unsafeRunSync
  println(s"result $result")

}
