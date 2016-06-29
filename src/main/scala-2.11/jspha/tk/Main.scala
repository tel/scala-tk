package jspha.tk

class Main extends App {

  // Create new safe effect-presentations using Safe
  def write(string: String): Tk.Safe[Unit] =
    Tk.Safe { println(string) }

  def read: Tk.Safe[String] =
    Tk.Safe { scala.io.StdIn.readLine() }

  // Effects which may respond exceptionally can be wrapped by
  // Tk and Tk.fn directly, capturing their exceptions
  // as values.
  //
  // If blowUp were defined using Unsafe.effThunk it would produce a
  // runtime exception.
  def blowUp: Tk[Throwable, Nothing] =
    Tk { throw new Exception("Oh no!") }

  // Compose Tk[A] values using for-comprehensions
  val tk: Tk[Throwable, Unit] =
    for {
      line <- write("Echo echo echo...") >> read
      ret <- blowUp >> write(line)
    } yield ret

  // Then, finally, when it's time to perform your effects do so
  // using the Run function.
  Tk.Run(tk)

}

