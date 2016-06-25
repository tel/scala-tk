package jspha.oi

/**
  * Safe "unsafe effect" wrapper monad similar to Scalaz's `IO` type.
  */
trait Task[+A] {

  def apply[R](pure: A => R, run: Effect.Runner[R]): R

  def map[B](f: A => B): Task[B] = Task.map(f)(this)
  def bind[B](k: A => Task[B]): Task[B] = Task.bind(k)(this)
  def flatMap[B](k: A => Task[B]): Task[B] = Task.bind(k)(this)

}

object Task {

  import Effect.{Runner, Partial}

  def eff[Req, Resp](ffi: Req => Resp)(request: Req): Task[Partial[Resp]] =
    new Task[Partial[Resp]] {
      def apply[R](pure: Partial[Resp] => R, run: Runner[R]) =
        run(Effect(ffi, request, pure))
    }

  def eff[Resp](ffi: => Resp): Task[Partial[Resp]] = new Task[Partial[Resp]] {
    def apply[R](pure: Partial[Resp] => R, run: Effect.Runner[R]) =
      run(Effect[Unit, Resp, R](_ => ffi, (), pure))
  }

  /**
    * Create an effect capturing all exceptions purely as an Option result.
    */
  def optionEff[Req, Resp](ffi: Req => Resp)(request: Req): Task[Option[Resp]] =
    new Task[Option[Resp]] {
      def apply[R](pure: Option[Resp] => R, run: Runner[R]) =
        run(Effect(ffi, request, {
          case Left(_) => pure(Option.empty)
          case Right(a) => pure(Option(a))
        }))
    }

  def optionEff[Resp](ffi: => Resp): Task[Option[Resp]] = new Task[Option[Resp]] {
    def apply[R](pure: Option[Resp] => R, run: Effect.Runner[R]) =
      run(Effect[Unit, Resp, R](_ => ffi, (), {
        case Left(_) => pure(Option.empty)
        case Right(a) => pure(Option(a))
      }))
  }

  def map[A, B](f: A => B)(fa: Task[A]): Task[B] = new Task[B] {
    def apply[R](pure: B => R, run: Effect.Runner[R]) =
      fa(pure compose f, run)
  }

  def pure[A](a: A): Task[A] = new Task[A] {
    def apply[R](pure: A => R, run: Effect.Runner[R]) = pure(a)
  }

  def ap[A, B](tf: Task[A => B])(ta: Task[A]): Task[B] = new Task[B] {
    def apply[R](pure: B => R, run: Runner[R]) =
      tf(f => ta(a => pure(f(a)), run), run)
  }

  def bind[A, B](k: A => Task[B])(fa: Task[A]): Task[B] = new Task[B] {
    def apply[R](pure: B => R, run: Effect.Runner[R]) =
      fa(k(_)(pure, run), run)
  }

  object Unsafe {

    /**
      * Extract the pure value, running all intermediate effects.
      */
    def peform[A](oi: Task[A]): A =
      oi[A](identity, Effect.Unsafe.perform[A])

    /**
      * Create an effect discarding the exception handler. You must ensure
      * that the effect cannot raise exceptions. If you violate this rule
      * then you may see exceptions thrown at construction time.
      */
    def eff[Req, Resp](ffi: Req => Resp)(request: Req): Task[Resp] =
      new Task[Resp] {
        def apply[R](pure: Resp => R, run: Runner[R]) =
          run(Effect(ffi, request, {
            case Left(throwable) => throw new RuntimeException(
              "Effect created via Unsafe.eff had exception!",
              throwable
            )
            case Right(a) => pure(a)
          }))
      }

    def eff[Resp](ffi: => Resp): Task[Resp] = new Task[Resp] {
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

