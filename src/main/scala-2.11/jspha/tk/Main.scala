package jspha.tk

class Main extends App {

  // Create new safe effect-presentations using Unsafe.eff
  def write(string: String): Tk.Pure[Unit] =
    Tk.Unsafe.eff(println)(string)

  // If your effect doesn't take an argument, Unsafe.effThunk may be
  // slightly more convenient.
  def read: Tk.Pure[String] =
    Tk.Unsafe {
      scala.io.StdIn.readLine()
    }

  // Effects which may respond exceptionally can be wrapped by
  // Tk.eff and Tk.effThunk directly, capturing their exceptions
  // as values.
  //
  // If blowUp were defined using Unsafe.effThunk it would produce a
  // runtime exception.
  def blowUp: Tk[Throwable, Nothing] =
    Tk {
      throw new Exception("Oh no!")
    }

  // Compose Tk[A] values using for-comprehensions
  val tk: Tk[Throwable, Unit] =
    for {
      _ <- write("Echo echo echo...").forgetE
      line <- read.forgetE
      _ <- blowUp
      _ <- write(line)
    } yield ()

  // Then, finally, when it's time to perform your effects do so
  // using the Unsafe.perform function.
  Tk.Unsafe.perform(tk)

}
