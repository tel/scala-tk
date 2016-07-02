package jspha.tk

import scala.language.higherKinds

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
trait Tk[F, +E, +A] {

  def apply[R](pure: A => R, fail: E => R, run: Eff.Runner[F, R]): R

//  def map[B](f: A => B): Tk[E, B] = Tk.map(f)(this)
//  def ap[EE >: E, B](f: Tk[EE, A => B]): Tk[EE, B] = Tk.ap(f)(this)
//  def bind[EE >: E, B](k: A => Tk[EE, B]): Tk[EE, B] = Tk.bind(k)(this)
//  def flatMap[EE >: E, B](k: A => Tk[EE, B]): Tk[EE, B] = Tk.bind(k)(this)
//
//  def >>[EE >: E, B](tk: Tk[EE, B]): Tk[EE, B] = bind(_ => tk)
//
//  def handle[F](f: E => F): Tk[F, A] = Tk.mapE(f)(this)
//  def caught: Tk[Nothing, Either[E, A]] = Tk.caught(this)
//  def caughtOption: Tk[Nothing, Option[A]] = Tk.caughtOption(this)

}

object Tk {

  import Eff.Runner

  def apply[A](ffi: => A): Tk[Eff, Throwable, A] =
    new Tk[Eff, Throwable, A] {
      def apply[R](pure: A => R, fail: Throwable => R, run: Runner[Eff, R]) =
        run(Eff[Unit, A, R](_ => ffi, (), fail, pure))
    }

  def fn[Req, Resp](ffi: Req => Resp)(request: Req): Tk[Eff, Throwable, Resp] =
    new Tk[Eff, Throwable, Resp] {
      def apply[R](pure: Resp => R, fail: Throwable => R, run: Runner[Eff, R]) =
        run(Eff[Req, Resp, R](ffi, request, fail, pure))
    }

// This doesn't quite work because we need to pipe the continuations in to
// the F[A] value, but they've already been fixed ahead of time. We're
// extending the wrong thing!
//
//  def eff[F[_] <: Eff, A](eff: F[A]): Tk[F, Throwable, A] =
//    new Tk[F, Throwable, A] {
//      def apply[R](pure: A => R, fail: Throwable => R, run: Runner[F, R]) =
//        run(eff)
//    }

  def pure[F, E, A](a: A): Tk[F, E, A] = new Tk[F, E, A] {
    def apply[R](pure: A => R, fail: E => R, run: Runner[F, R]) = pure(a)
  }

  def except[F, E, A](e: E): Tk[F, E, A] = new Tk[F, E, A] {
    def apply[R](pure: A => R, fail: E => R, run: Runner[F, R]) = fail(e)
  }

  def caught[F, E, A](tk: Tk[F, E, A]): Tk[F, Nothing, Either[E, A]] =
    new Tk[F, Nothing, Either[E, A]] {
    def apply[R](pure: Either[E, A] => R, fail: Nothing => R, run: Runner[F, R]) = {
      def newPure(a: A): R = pure(Right(a))
      def newFail(e: E): R = pure(Left(e))
      tk(newPure, newFail, run)
    }
  }

  def caughtOption[F, E, A](tk: Tk[F, E, A]): Tk[F, Nothing, Option[A]] =
    new Tk[F, Nothing, Option[A]] {
      def apply[R](pure: Option[A] => R, fail: Nothing => R, run: Runner[F, R]) = {
        def newPure(a: A): R = pure(Some(a))
        def newFail(e: E): R = pure(None)
        tk(newPure, newFail, run)
      }
    }

  def map[F, E, A, B](f: A => B)(fa: Tk[F, E, A]): Tk[F, E, B] =
    new Tk[F, E, B] {
      def apply[R](pure: B => R, fail: E => R, run: Runner[F, R]) =
        fa(f andThen pure, fail, run)
    }

  def mapE[F, E, A, EE](f: E => EE)(fa: Tk[F, E, A]): Tk[F, EE, A] =
    new Tk[F, EE, A] {
      def apply[R](pure: A => R, fail: EE => R, run: Runner[F, R]) =
        fa(pure, f andThen fail, run)
    }

  def ap[F, E, A, B](tf: Tk[F, E, A => B])(ta: Tk[F, E, A]): Tk[F, E, B] =
    new Tk[F, E, B] {
      def apply[R](pure: B => R, fail: E => R, run: Runner[F, R]) =
        tf(f => ta(a => pure(f(a)), fail, run), fail, run)
    }

  def bind[F, E, A, B](k: A => Tk[F, E, B])(fa: Tk[F, E, A]): Tk[F, E, B] =
    new Tk[F, E, B] {
      def apply[R](pure: B => R, fail: E => R, run: Runner[F, R]) =
        fa(k(_)(pure, fail, run), fail, run)
    }

  /**
    * A `Tk` is considered to be pure if no exceptions are thrown by effects
    * within it.
    */
  type Safe[F, A] = Tk[F, Nothing, A]

  object Safe {

    /**
      * Create an effect of no arguments identically to `fn`.
      */
    def apply[A](ffi: => A): Tk[Eff, Nothing, A] =
      new Tk[Eff, Nothing, A] {
        def apply[R](pure: A => R, fail: Nothing => R, run: Runner[Eff, R]) = {
          def thrower(t: Throwable) = throw t
          run(Eff[Unit, A, R](_ => ffi, (), thrower, pure))
        }
      }

    /**
      * Create an effect discarding the exception handler. You must ensure
      * that the effect cannot raise exceptions. If you violate this rule
      * then you may see exceptions thrown at runtime instead of being
      * propagated.
      */
    def fn[Req, Resp](ffi: Req => Resp)(request: Req): Tk[Eff, Nothing, Resp] =
      new Tk[Eff, Nothing, Resp] {
        def apply[R](pure: Resp => R, fail: Nothing => R, run: Runner[Eff, R]) = {
          def thrower(t: Throwable) = throw t
          run(Eff[Req, Resp, R](ffi, request, thrower, pure))
        }
      }
  }

  object Run {

    /**
      * Extract the result inner value, running all intermediate effects. If
      * any exceptions are encountered then they will be captured purely.
      */
    def apply[F <: Eff, E, A](tk: Tk[F, E, A]): Either[E, A] =
      tk(Right(_), Left(_), Eff.perform[F, Either[E, A]])

    /**
      * Extract the result inner value, running all intermediate effects. If
      * any exceptions are encountered then they will now be raised.
      */
    def throwing[F <: Eff, E <: Throwable, A](tk: Tk[F, E, A]): A = {
      def thrower(t: Throwable) = throw t
      tk(identity _, thrower, Eff.perform[F, A])
    }

    /**
      * Extract the result value, running all intermediate effects.
      */
    def pure[F <: Eff, A](tk: Safe[F, A]): A = {
      def elimNothing[R](n: Nothing): R = sys.error("Received Nothing")
      tk(identity _, elimNothing, Eff.perform[F, A])
    }

  }

}
