package jspha.tk

import scala.util.control.ControlThrowable

/**
  * An effect represents a "Thunked" effectful "FFI" call. Or, more
  * concretely, if `ffi` is an effectful Scala function `A => B` then
  * `Effect(ffi)` will be a *pure* Scala function `A => Effect[A, B,
  * Partial[B]]` indicating that when this effect is (later) performed it
  * will return a `B` so long as it doesn't hit an exception.
  */
case class EffectOld[Req, Resp, +R](ffi: Req => Resp,
                                 request: Req,
                                 fail: Throwable => R,
                                 continue: Resp => R)

object EffectOld {

  def apply[Req, Resp](ffi: Req => Resp)(req: Req): EffectOld[Req, Resp, Either[Throwable, Resp]] =
    EffectOld(ffi, req, Left(_), Right(_))

  trait Runner[R] {
    def apply[Req, Resp](eff: EffectOld[Req, Resp, R]): R
  }

  def perform[R]: Runner[R] = new Runner[R] {

    def apply[Req, Resp](eff: EffectOld[Req, Resp, R]): R =
      try {
        eff.continue(eff.ffi(eff.request))
      } catch {
        // We don't want to interfere with control, se
        // http://www.scala-lang.org/api/2.10.3/index.html#scala.util.control.ControlThrowable
        case e: ControlThrowable => throw e
        case e: Throwable => eff.fail(e)
      }
  }

}

