package com.joecordingley.mu

import cats.data.State
import monocle.{Lens, Optional}

import scala.language.implicitConversions

/**
  * Created by joe on 23/06/17.
  */
object StateHelper {

  implicit def lift[S, A, R](state: State[S, A])(
      implicit lens: Lens[R, S]): State[R, A] =
    state.transformS(lens.get, (r, s) => lens.modify(_ => s)(r))

}
