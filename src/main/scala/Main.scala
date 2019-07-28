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
    // val prog2 =for {
    //   s <- readC()
    //   _ <- printC(s)
    //   s2 <- readC()
    // } yield s2 + "world"
    // val res2 = handleC(prog2)
    // println(res2)

    import Eff._
    import Union._

    inline def fx[G[_], A](fa: (Option || G)[A], g: Option[A] => G[A]): G[A] = {
      fa match {
        case l: Option[A] => g(l)
        case r: G[A] => r
      }
    }

    val z: (Option || ([X] =>> Either[String, X]))[Int] = Some(3)
    println(fx(z, _.toRight("invalid")))
    // val z4: Double = 2.0
    // val z5: Double = f(z4, _.toDouble)

    // val z3: String = "aa"
    // val z2: String = f(z3, _.toString)

    println(ConsoleEff.example)
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

  // given ConsoleFunctor as Functor[Console] {
  //   import Console._
  //   def map[A, B](m: Console[A], f: A => B): Console[B] = m match {
  //     case Print(o, a) => Print(o, f(a))
  //     case Read(nf) => Read(f compose nf)
  //   }
  // }
  // def print(out: String): Free[Console, Unit] = impure[Console, Unit](Console.Print(out, ()))
  // def read(): Free[Console, String] = impure[Console, String](Console.Read(identity))

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
    // `type F[A] = Coyoneda[Console, A];  F[Console, Free[F, A]]` in Impure value
    // f: Any => Free[F, A]
    // fb: Console[Any]
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

object ConsoleEff {
  import Union._
  import EffOps._

  def print(out: String): Eff[Console || Void, Unit] = impure(Console.Print(out, ()))

  inline def example: Eff[Console || Void, Int] = for {
    _ <- print("hello.")
    _ <- print("world.")
  } yield 0
}
