package jspha.tk

import scala.language.higherKinds
import cats.{Functor, Monad, ~>}

/**
  * Continuation-passing style of a Free monad.
  */
trait CpsFree[F[_], A] { outer =>
  def apply[R](pure: A => R, wrap: F[R] => R): R

  final def map[B](f: A => B): CpsFree[F, B] =
    new CpsFree[F, B] {
      def apply[R](pure: B => R, wrap: F[R] => R) =
        outer(f andThen pure, wrap)
    }

  final def flatMap[B](k: A => CpsFree[F, B]): CpsFree[F, B] =
    new CpsFree[F, B] {
      def apply[R](pure: B => R, wrap: F[R] => R) =
        outer(a => k(a)(pure, wrap), wrap)
    }

  final def iter(k: F[A] => A): A =
    outer(identity, k)

  final def fold[M[_]: Monad](phi: F ~> M): M[A] = {
    val M = Monad[M]
    import M.{pure, flatten}
    outer(pure, (fma: F[M[A]]) => flatten(phi.apply(fma)))
  }

  final def hoist[G[_]](phi: F ~> G): CpsFree[G, A] =
    new CpsFree[G, A] {
      def apply[R](pure: A => R, wrap: G[R] => R) =
        outer(pure, (fr: F[R]) => wrap(phi(fr)))
    }

}

object CpsFree extends CpsFreeInstances {

  def liftF[F[_]: Functor, A](fa: F[A]): CpsFree[F, A] =
    new CpsFree[F, A] {
      def apply[R](pure: A => R, wrap: F[R] => R) =
        wrap(Functor[F].map(fa)(pure))
    }

}

private [tk] sealed abstract class CpsFreeInstances {

  def cpsFreeMonad[F[_]: Functor]: Monad[({ type l[A] = CpsFree[F, A]})#l] =
    new Monad[({type l[A] = CpsFree[F, A]})#l] {

      def pure[A](x: A) =
        new CpsFree[F, A] {
          def apply[R](pure: A => R, wrap: F[R] => R) = pure(x)
        }

      def flatMap[A, B](fa: CpsFree[F, A])(f: A => CpsFree[F, B]) =
        fa.flatMap(f)
    }

}
