object Main {
  def main(args: Array[String]): Unit = {
    import FreeOps._
    import delegate OptionOps._
    import OptionOps._

    val res =for {
      a <- some(3)
      b <- some(a + 2)
      _ <- none()
    } yield b.toString
    println(res)

    import delegate ConsoleOps._
    import ConsoleOps._

    // val prog =for {
    //   s <- read()
    //   _ <- print(s)
    //   s2 <- read()
    // } yield s2 + "world"
    // println(prog)
    // val lst = handle(prog)
    // println(lst)

    import delegate CoyonedaOps._
    val prog2 =for {
      s <- readC()
      _ <- printC(s)
      s2 <- readC()
    } yield s2 + "world"
    val res2 = handleC(prog2)
    println(res2)
  }
}

object OptionOps {
  import FreeOps._

  given OptionFunctor as Functor[Option] {
    def map[A, B](m: Option[A], f: A => B): Option[B] = m match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }

  def some[A](a: A): Free[Option, A] = impure(Some(a))
  def none[A](): Free[Option, A] = impure(None)
}
enum Console[+A] {
  case Print(out: String, next: A)
  case Read(next: String => A)
}

object ConsoleOps {
  import FreeOps._

  given ConsoleFunctor as Functor[Console] {
    import Console._
    def map[A, B](m: Console[A], f: A => B): Console[B] = m match {
      case Print(o, a) => Print(o, f(a))
      case Read(nf) => Read(f compose nf)
    }
  }
  def print(out: String): Free[Console, Unit] = impure[Console, Unit](Console.Print(out, ()))
  def read(): Free[Console, String] = impure[Console, String](Console.Read(identity))

  import CoyonedaOps._
  def printC(out: String): Free[CX[Console], Unit] = impureC[Console, Unit](Console.Print(out, ()))
  def readC(): Free[CX[Console], String] = impureC[Console, String](Console.Read(identity))

  def handle[A](fm: Free[Console, A]): A = fm match {
    case Free.Pure(a) => a
    case Free.Impure(im) => im match {
      case Console.Print(o, n) =>
        println(o)
        handle(n)
      case Console.Read(f) =>
        val in = io.StdIn.readLine()
        handle { f(in) }
    }
  }

  def handleC[A](fm: Free[CX[Console], A]): A = fm match {
    case Free.Pure(a) => a
    case Free.Impure(Coyoneda(f, fb)) => fb match {
      case Console.Print(o, a) =>
        println(o)
        handleC { f(a) }
      case Console.Read(fr) =>
        val in = io.StdIn.readLine()
        handleC { f(fr(in)) }
    }
  }
}
