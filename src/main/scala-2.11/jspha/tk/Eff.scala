package jspha.tk

import scala.util.control.ControlThrowable
import scala.language.higherKinds

trait Eff[R] {

  type Request
  type Response

  val request: Request
  def apply(req: Request): Response
  def fail(t: Throwable): R
  def continue(resp: Response): R

}

object Eff {

  def apply[Req, Resp, R](ffi: Req => Resp,
                          req: Req,
                          failf: Throwable => R,
                          contf: Resp => R): Eff[R] =
    new Eff[R] {
      type Request = Req
      type Response = Resp

      val request = req
      def apply(r: Req) = ffi(r)
      def fail(t: Throwable) = failf(t)
      def continue(r: Resp) = contf(r)
    }

  def apply[Req, Resp](ffi: Req => Resp)(req: Req): Eff[Either[Throwable, Resp]] =
    apply[Req, Resp, Either[Throwable, Resp]](ffi, req, Left(_), Right(_))

  trait Runner[F[_], R] {
    def apply[Req, Resp](eff: F[R]): R
  }

  def perform[F[_] <: Eff[R], R]: Runner[F, R] = new Runner[F, R] {

    def apply[Req, Resp](eff: F[R]): R =
      try {
        eff.continue(eff(eff.request))
      } catch {
        // We don't want to interfere with control, se
        // http://www.scala-lang.org/api/2.10.3/index.html#scala.util.control.ControlThrowable
        case e: ControlThrowable => throw e
        case e: Throwable => eff.fail(e)
      }
  }

}