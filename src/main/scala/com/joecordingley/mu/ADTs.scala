package com.joecordingley.mu

sealed trait CardPlayADT[A]


sealed trait TrickPlayADT[A] extends CardPlayADT[A]
