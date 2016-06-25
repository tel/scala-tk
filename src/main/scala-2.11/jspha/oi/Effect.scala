package jspha.oi

/**
  * An effect represents a "Thunked" effectful "FFI" call. Or, more
  * concretely, if `ffi` is an effectful Scala function `A => B` then
  * `Effect(ffi)` will be a *pure* Scala function `A => Effect[A, B,
  * Partial[B]]` indicating that when this effect is (later) performed it
  * will return a `B` so long as it doesn't hit an exception.
  */
case class Effect[-Req, +Resp, +A](ffi: Req => Resp,
                                   request: Req,
                                   continue: Either[Throwable, Resp] => A) {

  def map[B](f: A => B): Effect[Req, Resp, B] =
    Effect.map(f)(this)

  def observing: Effect[Req, Resp, Either[Throwable, A]] =
    Effect.observing(this)

}

object Effect {

  /**
    * Partial responses may not actually arrive.
    */
  type Partial[+Resp] = Either[Throwable, Resp]

  /**
    * Eliminator for Effects ignorant to Request and Response type.
    */
  trait Runner[A] {
    def apply[Req, Resp](eff: Effect[Req, Resp, A]): A
  }

  /**
    * Create a raw effect, choosing how to interpret the FFI response.
    */
  protected def apply[Req, Resp, A](ffi: Req => Resp,
                                    request: Req,
                                    continue: Partial[Resp] => A): Effect[Req, Resp, A] =
    new Effect[Req, Resp, A](ffi, request, continue)

  /**
    * Create a parameterized effect.
    */
  def apply[Req, Resp](ffi: Req => Resp)(request: Req): Effect[Req, Resp, Partial[Resp]] =
    apply[Req, Resp, Partial[Resp]](ffi, request, identity)

  /**
    * Create an effect from a thunk.
    */
  def apply[Resp](ffi: => Resp): Effect[Unit, Resp, Partial[Resp]] =
    apply[Unit, Resp, Partial[Resp]](_ => ffi, (), identity)

  /**
    * Transform the return parameter of an Effect
    */
  def map[Req, Resp, A, B](f: A => B)(fa: Effect[Req, Resp, A]): Effect[Req, Resp, B] =
    Effect(fa.ffi, fa.request, fa.continue andThen f)

  /**
    * Capture exceptions immediately.
    */
  def observing[Req, Resp, A](fa: Effect[Req, Resp, A]): Effect[Req, Resp, Partial[A]] =
    Effect(fa.ffi, fa.request, {
      case Left(thrown) => Left(thrown)
      case Right(a) => Right(fa.continue(Right(a)))
    })

  object Unsafe {

    /**
      * Performs an effect handling exceptions.
      */
    def perform[Req, Resp, A](eff: Effect[Req, Resp, A]): A =
      try {
        eff.continue(Right(eff.ffi(eff.request)))
      } catch {
        case thrown: Throwable => eff.continue(Left(thrown))
      }

    def perform[A]: Runner[A] = new Runner[A] {
      def apply[Req, Resp](eff: Effect[Req, Resp, A]) =
        perform(eff)
    }

  }

}