trait Functor[F[_]] {
  def map[A, B](m: F[A], f: A => B): F[B]
}

sealed trait Free[F[_], A]
object Free {
  case class Pure[F[_], A](v: A) extends Free[F, A]
  case class Impure[F[_], A](m: F[Free[F, A]]) extends Free[F, A]

  def map[F[_]: Functor, A, B](m: Free[F, A], f: A => B): Free[F, B] = m match {
    case Pure(v) => Pure(f(v))
    case Impure(m) => {
      val res = the[Functor[F]].map(m, m2 => map(m2, f)) // 何回層で包まれていようと結局は値は一つなので最下部の値に対して関数を一度だけ適用すればよい
      Impure(res)
    }
  }
  def flatMap[F[_]: Functor, A, B](m: Free[F, A], f: A => Free[F, B]): Free[F, B] = m match {
    case Pure(v) => f(v)
    case Impure(m) =>
      val res = the[Functor[F]].map(m, m2 => flatMap(m2, f))
      Impure(res)
  }
}

object FreeOps {
  def pure[F[_], A](a: A): Free[F, A] = Free.Pure(a)
  def impure[F[_]: Functor, A](fa: F[A]): Free[F, A] = Free.Impure { the[Functor[F]].map(fa, pure) }

  import CoyonedaOps._
  import delegate CoyonedaOps._

  def impureC[F[_], A](fa: F[A]): Free[CX[F], A] = Free.Impure {
    the[Functor[CX[F]]].map(coyoneda(fa), a => pure(a))
  }

  def (fm: Free[F, A]) map[F[_]: Functor, A, B] (f: A => B): Free[F, B] = Free.map(fm, f)
  def (fm: Free[F, A]) flatMap[F[_]: Functor, A, B] (f: A => Free[F, B]): Free[F, B] = Free.flatMap(fm, f)
}

case class Coyoneda[F[_], A](f: Any => A, fb: F[Any])

object CoyonedaOps {
  type CX[F[_]] = [X] =>> Coyoneda[F, X]
  implicit def CoyonedaFunctor[F[_]]: Functor[CX[F]] = new Functor[CX[F]] {
    def map[A, B](m: Coyoneda[F, A], f: A => B): Coyoneda[F, B] = Coyoneda(f compose m.f, m.fb)
  }

  def coyoneda[F[_], B](fb: F[B]): Coyoneda[F, B] = Coyoneda(a => a.asInstanceOf[B], fb.asInstanceOf[F[Any]])
}
