
# *Oi!* message-passing effect monad

*Oi* aims to be an efficient, well-behaved "impure effect wrapper 
monad" in the same vein as Scalaz's `IO` and `Task` types. You can 
use it to transform side-effecting, imperative APIs into ones which 
can be combined purely and then executed all at once later.

## Example

tk.

## Theory

OI's `Tk` ("task" or "to come" in newspaper editor speak) is a 
certain kind of free monad over the *indexed store* 
comonad. This provides *message-passing* semantics to "interacting 
with the real world" which are amenable to good models for 
concurrent threaded computation (unlike `RealWorld` token passing 
models if you're familiar with them).

In particular, `Tk` is the flattened form of a "codensity 
transformed" Free monad structure applied to the Store comonad. 
Even more particularly, it's not actually the `Codensity` of `Free`
but instead a smaller structure with all of the desired properties 
arising from applying `Yoneda` instead.

To understand `Tk` best, we can look at a simplified model of
the free monad over the indexed store comonad

```scala
sealed trait Tkish[+A]
case class Pure[+A](a: A) extends Tkish[A]
case class Effect[-Req, +Resp, +A](
  ffi: FFIOperation[Request, Response],
  request: Request,
  continue: Response => Tkish[A]
) extends Tkish[A]
```

This has the shape of a free monad embedding either a pure value 
with `Pure` or a wrapper of effects with `Effect`. In the case of 
`Effect` we see that we have all of the required components to 
perform some FFI (impure, basic) operation which converts requests 
of type `Request` to responses of type `Response` after which we use
that response to continue the computation.

Assuming we have a method to send and receive messages to the 
runtime using the `FFIOperation` type the interpretation behavior of 
`Tkish` is now direct. Then, given that Scala is natively an 
effectful language we immediately have

```scala
type FFIOperation[-Req, +Resp] = Req => Resp
```

In practice, the actual `Tk` monad is identical to this except (a)
it uses a Yoneda transformed version of the Free monad for 
efficiency and (b) it also contains an exception handler to properly
account for the exceptions that may be thrown by `FFIOperation`s.

### References and sources

Most of these ideas were elaborated by Ed Kmett in a series of posts 
on https://comonad.com called "Free Monads for Less". Within these 
posts Ed references many relevant papers describing these techniques.
The translation to Scala is novel (to me).

- http://comonad.com/reader/2011/free-monads-for-less/
- http://comonad.com/reader/2011/free-monads-for-less-2/
- http://comonad.com/reader/2011/free-monads-for-less-3/