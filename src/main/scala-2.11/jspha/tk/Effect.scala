package jspha.tk

/**
  * An effect represents a "Thunked" effectful "FFI" call. Or, more
  * concretely, if `ffi` is an effectful Scala function `A => B` then
  * `Effect(ffi)` will be a *pure* Scala function `A => Effect[A, B,
  * Partial[B]]` indicating that when this effect is (later) performed it
  * will return a `B` so long as it doesn't hit an exception.
  */
case class Effect[Req, Resp, +A](ffi: Req => Resp,
                                 request: Req,
                                 fail: Throwable => A,
                                 continue: Resp => A)

object Effect {

  trait Runner[R] {
    def apply[Req, Resp](eff: Effect[Req, Resp, R]): R
  }

  def perform[R]: Runner[R] = new Runner[R] {

    def apply[Req, Resp](eff: Effect[Req, Resp, R]): R =
      try {
        eff.continue(eff.ffi(eff.request))
      } catch {
        case e: Throwable => eff.fail(e)
      }
  }

}

