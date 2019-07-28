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

  def (fm: Free[F, A]) map[F[_]: Functor, A, B] (f: A => B): Free[F, B] = Free.map(fm, f)
  def (fm: Free[F, A]) flatMap[F[_]: Functor, A, B] (f: A => Free[F, B]): Free[F, B] = Free.flatMap(fm, f)

  import CoyonedaOps._
  def impureC[F[_], A](fa: F[A]): Free[CX[F], A] = Free.Impure {
    the[Functor[CX[F]]].map(coyoneda(fa), a => pure(a))
  }
}

case class Coyoneda[F[_], A](f: Any => A, fb: F[Any])

object CoyonedaOps {
  type CX[F[_]] = [X] =>> Coyoneda[F, X]
  implicit def CoyonedaFunctor[F[_]]: Functor[CX[F]] = new Functor[CX[F]] {
    def map[A, B](m: Coyoneda[F, A], f: A => B): Coyoneda[F, B] = Coyoneda(f compose m.f, m.fb)
  }

  def coyoneda[F[_], B](fb: F[B]): Coyoneda[F, B] = Coyoneda(a => a.asInstanceOf[B], fb.asInstanceOf[F[Any]])
}

sealed trait Union[F[_], G[_], A]
object Union {
  case class Inl[F[_], G[_], A](value: F[A]) extends Union[F, G, A]
  case class Inr[F[_], G[_], A](value: G[A]) extends Union[F, G, A]
  final abstract class Void[A]

  type :+:[F[_], G[_]] = [A] =>> Union[F, G, A]
}

trait Member[F[_], G[_]] {
  def inject[A](f: F[A]): G[A]
}
object Member {
  import Union._
  implicit def left[F[_], G[_]]: Member[F, F :+: G] =
    new Member[F, F :+: G] {
      def inject[A](fa: F[A]): (F :+: G)[A] = Inl(fa)
    }

  implicit def right[F[_], G[_], H[_]](implicit member: Member[F, H]): Member[F, G :+: H] =
    new Member[F, G :+: H] {
      def inject[A](fa: F[A]): (G :+: H)[A] = Inr(member.inject(fa))
    }
}

sealed trait Eff[R[_], A]

object Eff {
  case class Pure[R[_], A](v: A) extends Eff[R, A]
  case class Impure[R[_], A](fz: R[Any], f: Any => Eff[R, A]) extends Eff[R, A]

  def map[F[_], A, B](m: Eff[F, A], f: A => B): Eff[F, B] = m match {
    case Pure(v) => Pure(f(v))
    case Impure(fz, fo) => Impure(fz, a => map(fo(a), f))
  }
  import Union._
  def flatMap[F[_], A, B](m: Eff[F, A], f: A => Eff[F, B]): Eff[F, B] = m match {
    case Pure(v) => f(v)
    case Impure(fz, fo) =>
      Impure(fz, a => {
        val n = fo(a)
        flatMap(n, f)
      })
  }
}

object EffOps {
  import Union._
  def pure[F[_], A](a: A): Eff[F, A] = Eff.Pure(a)
  def impure[F[_], R[_], A](fa: F[A]) given (m: Member[F, R]): Eff[R, A] = Eff.Impure(m.inject(fa.asInstanceOf[F[Any]]), a => pure(a.asInstanceOf[A]))

  import Union._
  def (fm: Eff[F, A]) map[F[_], A, B] (f: A => B): Eff[F, B] = Eff.map(fm, f)
  def (fm: Eff[F, A]) flatMap[F[_], A, B] (f: A => Eff[F, B]): Eff[F, B] = Eff.flatMap(fm, f)
}
