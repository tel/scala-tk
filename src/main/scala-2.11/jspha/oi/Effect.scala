package jspha.oi

case class Effect[-Req, +Resp, +A](ffi: Req => Resp,
                                   request: Req,
                                   inject: Either[Throwable, Resp] => A) {

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

  def part[Resp](t: Throwable): Partial[Resp] = Left(t)
  def here[Resp](r: Resp): Partial[Resp] = Right(r)

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
                                    inject: Partial[Resp] => A): Effect[Req, Resp, A] =
    new Effect[Req, Resp, A](ffi, request, inject)

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
    Effect(fa.ffi, fa.request, fa.inject andThen f)

  /**
    * Capture exceptions immediately.
    */
  def observing[Req, Resp, A](fa: Effect[Req, Resp, A]): Effect[Req, Resp, Partial[A]] =
    Effect(fa.ffi, fa.request, {
      case Left(thrown) => Left(thrown)
      case Right(a) => Right(fa.inject(Right(a)))
    })

  object Unsafe {

    /**
      * Performs an effect handling exceptions.
      */
    def perform[Req, Resp, A](eff: Effect[Req, Resp, A]): A =
      try {
        eff.inject(Right(eff.ffi(eff.request)))
      } catch {
        case thrown: Throwable => eff.inject(Left(thrown))
      }

    def perform[A]: Runner[A] = new Runner[A] {
      def apply[Req, Resp](eff: Effect[Req, Resp, A]) =
        perform(eff)
    }

  }

}