package jspha.tk

import scala.language.higherKinds
import cats.MonadError
import cats.functor.Bifunctor

/**
  * Safe "unsafe effect" wrapper monad similar to Scalaz's `IO` type.
  * Values of `Tk[A]` may be pure computations returning values `A` or
  * chains of effectful computations which *eventually* return values `A`
  * if they proceed unexceptionally.
  *
  * Basic, foundational elements of these `Tk` effect-chains can be built
  * using the `Tk` and `Tk.Safe` combinators which wrap standard,
  * side-effecting, unsafe Scala functions.
  *
  * When it is time to ultimately perform your `Tk` action to receive the
  * result value you can use the function `Tk.Unsafe.perform` to run the
  * effect chain and return the value.
  */
trait Tk[+E, +A] { outer =>

  import Effect.Runner

  def apply[R](pure: A => R, fail: E => R, run: Effect.Runner[R]): R

  //

  def map[B](f: A => B): Tk[E, B] =
    new Tk[E, B] {
      def apply[R](pure: B => R, fail: E => R, run: Runner[R]) =
        outer(f andThen pure, fail, run)
    }

  def ap[EE >: E, B](tf: Tk[EE, A => B]): Tk[EE, B] =
    new Tk[EE, B] {
      def apply[R](pure: B => R, fail: EE => R, run: Runner[R]) =
        tf((f: A => B) => outer((a: A) => pure(f(a)), fail, run), fail, run)
    }


  def flatMap[EE >: E, B](k: A => Tk[EE, B]): Tk[EE, B] =
    new Tk[EE, B] {
      def apply[R](pure: B => R, fail: EE => R, run: Effect.Runner[R]) =
        outer(k(_)(pure, fail, run), fail, run)
    }

  def >>[EE >: E, B](tk: Tk[EE, B]): Tk[EE, B] =
    flatMap(_ => tk)

  def handle[F](f: E => F): Tk[F, A] =
    new Tk[F, A] {
      def apply[R](pure: A => R, fail: F => R, run: Runner[R]) =
        outer(pure, f andThen fail, run)
    }

  def caught: Tk[Nothing, Either[E, A]] =
    new Tk[Nothing, Either[E, A]] {
      def apply[R](pure: Either[E, A] => R, fail: Nothing => R, run: Runner[R]) = {
        def newPure(a: A): R = pure(Right(a))
        def newFail(e: E): R = pure(Left(e))
        outer(newPure, newFail, run)
      }
    }

  def caughtOption: Tk[Nothing, Option[A]] =
    new Tk[Nothing, Option[A]] {
      def apply[R](pure: Option[A] => R, fail: Nothing => R, run: Runner[R]) = {
        def newPure(a: A): R = pure(Some(a))
        def newFail(e: E): R = pure(None)
        outer(newPure, newFail, run)
      }
    }

}

object Tk extends TkInstances with TkFunctions {

  /**
    * A `Tk` is considered to be pure if no exceptions are thrown by effects
    * within it.
    */
  type Safe[A] = Tk[Nothing, A]

  object Safe {

    import Effect.Runner

    /**
      * Create an effect of no arguments identically to `fn`.
      */
    def apply[A](ffi: => A): Tk[Nothing, A] =
      new Tk[Nothing, A] {
        def apply[R](pure: A => R, fail: Nothing => R, run: Runner[R]) = {
          def thrower(t: Throwable) = throw t
          run(Effect[Unit, A, R](_ => ffi, (), thrower, pure))
        }
      }

    /**
      * Create an effect discarding the exception handler. You must ensure
      * that the effect cannot raise exceptions. If you violate this rule
      * then you may see exceptions thrown at runtime instead of being
      * propagated.
      */
    def fn[Req, Resp](ffi: Req => Resp)(request: Req): Tk[Nothing, Resp] =
      new Tk[Nothing, Resp] {
        def apply[R](pure: Resp => R, fail: Nothing => R, run: Runner[R]) = {
          def thrower(t: Throwable) = throw t
          run(Effect[Req, Resp, R](ffi, request, thrower, pure))
        }
      }
  }

  object Run {

    /**
      * Extract the result inner value, running all intermediate effects. If
      * any exceptions are encountered then they will be captured purely.
      */
    def apply[E, A](tk: Tk[E, A]): Either[E, A] =
      tk(Right(_), Left(_), Effect.perform)

    /**
      * Extract the result inner value, running all intermediate effects. If
      * any exceptions are encountered then they will now be raised.
      */
    def throwing[E <: Throwable, A](tk: Tk[E, A]): A = {
      def thrower(t: Throwable) = throw t
      tk(identity _, thrower, Effect.perform)
    }

    /**
      * Extract the result value, running all intermediate effects.
      */
    def pure[A](tk: Safe[A]): A = {
      def elimNothing[R](n: Nothing): R = sys.error("Received Nothing")
      tk(identity _, elimNothing, Effect.perform)
    }

  }

}

trait TkFunctions {

  import Effect.Runner

  def apply[A](ffi: => A): Tk[Throwable, A] =
    new Tk[Throwable, A] {
      def apply[R](pure: A => R, fail: Throwable => R, run: Effect.Runner[R]) =
        run(Effect[Unit, A, R](_ => ffi, (), fail, pure))
    }


  def fn[Req, Resp](ffi: Req => Resp)(request: Req): Tk[Throwable, Resp] =
    new Tk[Throwable, Resp] {
      def apply[R](pure: Resp => R, fail: Throwable => R, run: Runner[R]) =
        run(Effect[Req, Resp, R](ffi, request, fail, pure))
    }

}

private[tk] sealed abstract class TkInstances {

  import Effect.Runner

  implicit object bifunctor extends Bifunctor[Tk] {
    def bimap[A, B, C, D](fab: Tk[A, B])(f: A => C, g: B => D): Tk[C, D] =
      new Tk[C, D] {
        def apply[R](pure: (D) => R, fail: (C) => R, run: Runner[R]) =
          fab(g andThen pure, f andThen fail, run)
      }
  }

  implicit def monadError[E]: MonadError[({type l[A] = Tk[E, A]})#l, E] =
    new MonadError[({type l[A] = Tk[E, A]})#l, E] {

      def flatMap[A, B](fa: Tk[E, A])(f: A => Tk[E, B]) =
        fa.flatMap(f)

      def handleErrorWith[A](fa: Tk[E, A])(f: E => Tk[E, A]) =
        new Tk[E, A] {
          def apply[R](pure: A => R, fail: E => R, run: Runner[R]) =
            fa(pure, e => f(e)(pure, fail, run), run)
        }

      def raiseError[A](e: E) =
        new Tk[E, A] {
          def apply[R](pure: A => R, fail: E => R, run: Runner[R]) =
            fail(e)
        }

      def pure[A](x: A) =
        new Tk[E, A] {
          def apply[R](pure: A => R, fail: E => R, run: Runner[R]) =
            pure(x)
        }

    }

}
