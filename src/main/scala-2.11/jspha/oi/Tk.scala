package jspha.oi

/**
  * Safe "unsafe effect" wrapper monad similar to Scalaz's `IO` type.
  */
trait Tk[+A] {

  def apply[R](pure: A => R, run: Effect.Runner[R]): R

  def map[B](f: A => B): Tk[B] = Tk.map(f)(this)
  def bind[B](k: A => Tk[B]): Tk[B] = Tk.bind(k)(this)
  def flatMap[B](k: A => Tk[B]): Tk[B] = Tk.bind(k)(this)

}

object Tk {

  import Effect.{Runner, Partial}

  def eff[Req, Resp](ffi: Req => Resp)(request: Req): Tk[Partial[Resp]] =
    new Tk[Partial[Resp]] {
      def apply[R](pure: Partial[Resp] => R, run: Runner[R]) =
        run(Effect(ffi, request, pure))
    }

  def eff[Resp](ffi: => Resp): Tk[Partial[Resp]] = new Tk[Partial[Resp]] {
    def apply[R](pure: Partial[Resp] => R, run: Effect.Runner[R]) =
      run(Effect[Unit, Resp, R](_ => ffi, (), pure))
  }

  /**
    * Create an effect capturing all exceptions purely as an Option result.
    */
  def optionEff[Req, Resp](ffi: Req => Resp)(request: Req): Tk[Option[Resp]] =
    new Tk[Option[Resp]] {
      def apply[R](pure: Option[Resp] => R, run: Runner[R]) =
        run(Effect(ffi, request, {
          case Left(_) => pure(Option.empty)
          case Right(a) => pure(Option(a))
        }))
    }

  def optionEff[Resp](ffi: => Resp): Tk[Option[Resp]] = new Tk[Option[Resp]] {
    def apply[R](pure: Option[Resp] => R, run: Effect.Runner[R]) =
      run(Effect[Unit, Resp, R](_ => ffi, (), {
        case Left(_) => pure(Option.empty)
        case Right(a) => pure(Option(a))
      }))
  }

  def map[A, B](f: A => B)(fa: Tk[A]): Tk[B] = new Tk[B] {
    def apply[R](pure: B => R, run: Effect.Runner[R]) =
      fa(pure compose f, run)
  }

  def pure[A](a: A): Tk[A] = new Tk[A] {
    def apply[R](pure: A => R, run: Effect.Runner[R]) = pure(a)
  }

  def ap[A, B](tf: Tk[A => B])(ta: Tk[A]): Tk[B] = new Tk[B] {
    def apply[R](pure: B => R, run: Runner[R]) =
      tf(f => ta(a => pure(f(a)), run), run)
  }

  def bind[A, B](k: A => Tk[B])(fa: Tk[A]): Tk[B] = new Tk[B] {
    def apply[R](pure: B => R, run: Effect.Runner[R]) =
      fa(k(_)(pure, run), run)
  }

  object Unsafe {

    /**
      * Extract the pure value, running all intermediate effects.
      */
    def peform[A](oi: Tk[A]): A =
      oi[A](identity, Effect.Unsafe.perform[A])

    /**
      * Create an effect discarding the exception handler. You must ensure
      * that the effect cannot raise exceptions. If you violate this rule
      * then you may see exceptions thrown at construction time.
      */
    def eff[Req, Resp](ffi: Req => Resp)(request: Req): Tk[Resp] =
      new Tk[Resp] {
        def apply[R](pure: Resp => R, run: Runner[R]) =
          run(Effect(ffi, request, {
            case Left(throwable) => throw new RuntimeException(
              "Effect created via Unsafe.eff had exception!",
              throwable
            )
            case Right(a) => pure(a)
          }))
      }

    def eff[Resp](ffi: => Resp): Tk[Resp] = new Tk[Resp] {
      def apply[R](pure: Resp => R, run: Effect.Runner[R]) =
        run(Effect[Unit, Resp, R](_ => ffi, (), {
          case Left(throwable) => throw new RuntimeException(
            "Effect created via Unsafe.eff had exception!",
            throwable
          )
          case Right(a) => pure(a)
        }))
    }
  }

}

