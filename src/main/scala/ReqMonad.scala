trait ResUnion[+A]

object ResUnion {
  case class ResOk[+A](res: A) extends ResUnion[A]
  trait ResFail extends ResUnion[Nothing]
}
trait ServiceA[A]
object ServiceA {
  import ResUnion._
  import Union._
  import Member._
  import RPCMonad._
  import RPCMonadOps._


  class Conf

  case class CallFunctionA[A](in: String) extends ServiceA[A]

  def callFunctionA[R[_]](in: String) given Member[ServiceA, R]: RPCMonad[R, Int] = req[ServiceA, R, Int](CallFunctionA(in))
  def callFunctionAImpl(in: String): Either[String, Int] = {
    println(s"== request to ${in}... ==")
    Right(3)
  }

  def handle[A, R[_]](fm: RPCMonad[ServiceA :+: R, A])(conf: Conf): RPCMonad[R, A] = fm match {
    case Ok(a) => Ok(a)
    case Request(Inl(CallFunctionA(in)), f) =>
      callFunctionAImpl(in) match {
        case Right(v) => handle { f(v) }(conf)
        case Left(_) => new Fail {}
      }
    case Request(Inr(inr), f) => Request(inr, a => handle { f(a) }(conf))
  }
}

trait ServiceB[A]
object ServiceB {
  import ResUnion._
  import Union._
  import Member._
  import RPCMonad._
  import RPCMonadOps._

  case class CallFunctionA[A](in: Int) extends ServiceB[A]

  def callFunctionA[R[_]](in: Int) given Member[ServiceB, R]: RPCMonad[R, String] = req[ServiceB, R, String](CallFunctionA(in))

  def callFunctionAImpl(in: Int): Either[String, String] = {
    println(s"== request service B to ${in}... ==")
    Right(s"== Ok $in ==")
  }

  def handle[A, R[_]](fm: RPCMonad[ServiceB :+: R, A]): RPCMonad[R, A] = fm match {
    case Ok(a) => Ok(a)
    case Request(Inl(CallFunctionA(in)), f) =>
      callFunctionAImpl(in) match {
        case Right(v) => handle { f(v) }
        case Left(_) => new Fail {}
      }
    case Request(Inr(inr), f) => Request(inr, a => handle { f(a) })
  }
}

sealed trait RPCMonad[R[_], +A]

object RPCMonad {
  case class Ok[R[_], +A](v: A) extends RPCMonad[R, A]
  trait Fail[R[_]] extends RPCMonad[R, Nothing]

  case class Request[R[_], +A](fz: R[Any], f: Any => RPCMonad[R, A]) extends RPCMonad[R, A]

  def map[F[_], A, B](m: RPCMonad[F, A], f: A => B): RPCMonad[F, B] = m match {
    case Ok(v) => Ok(f(v))
    case fa: Fail[F] => new Fail {}
    case Request(fz, fo) => Request(fz, a => map(fo(a), f))
  }
  import Union._
  def flatMap[F[_], A, B](m: RPCMonad[F, A], f: A => RPCMonad[F, B]): RPCMonad[F, B] = m match {
    case Ok(v) => f(v)
    case fa: Fail[F] => new Fail {}
    case Request(fz, fo) =>
      Request(fz, a => {
        val n = fo(a)
        flatMap(n, f)
      })
  }
}

object RPCMonadOps {
  import Union._
  def ok[F[_], A](a: A): RPCMonad[F, A] = RPCMonad.Ok(a)
  def req[F[_], R[_], A](fa: F[A]) given (m: Member[F, R]): RPCMonad[R, A] = RPCMonad.Request(m.inject(fa.asInstanceOf[F[Any]]), a => ok(a.asInstanceOf[A]))

  import Union._
  def (fm: RPCMonad[F, A]) map[F[_], A, B] (f: A => B): RPCMonad[F, B] = RPCMonad.map(fm, f)
  def (fm: RPCMonad[F, A]) flatMap[F[_], A, B] (f: A => RPCMonad[F, B]): RPCMonad[F, B] = RPCMonad.flatMap(fm, f)
}
