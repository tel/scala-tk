package jspha.oi

class Main extends App {

  def write(string: String): Tk[Unit] =
    Tk.Unsafe.eff(println)(string)

  def read: Tk[String] =
    Tk.Unsafe.effThunk {
      scala.io.StdIn.readLine()
    }

  val tk: Tk[Unit] =
    for {
      line <- read
      write(line)
    } yield ()

  override def main(args: Array[String]): Unit = {
    Tk.Unsafe.perform(tk)
  }

}
