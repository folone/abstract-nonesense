package free.applicative

import scalaz._

/** The free Applicative for a Functor `F`. */
sealed abstract class FreeA[F[+_], +A](implicit F: Functor[F]) {
  import FreeA._
  import syntax.applicative._
  import syntax.std.function2._
  def run[B, G[+_]: Applicative](u: F[B] ⇒ G[B]): G[A] =
    this match {
      case Pure(x)        ⇒ Applicative[G].pure(x)
      case Ap(f: F[B], x) ⇒ u(f) <*> x.run(u)
    }
  def hoistAp[F[+_]: Functor, G[+_]: Functor, B](f: F[B] ⇒ G[B]): FreeA[G, A] =
    this match {
      case Pure(a)        ⇒ Pure(a)
      case Ap(x: F[B], y) ⇒ Ap(f(x), y.hoistAp(f))
    }
  def map[B](f: A ⇒ B): FreeA[F, B] =
    this match {
      case Pure(x)           ⇒ Pure(f(x))
      case Ap(fa: F[A], apf) ⇒ Ap(fa, apf map (f compose _))
    }
  def <*>[B](fa: FreeA[F, A ⇒ B]): FreeA[F, B] =
    this match {
      case Pure(f)        ⇒ fa.map(fun ⇒ fun(f))
      // FIXME having problems implementing the following
      //case Ap(x: F[A], y) ⇒ Ap(x, fa <*> y.map(_.flip))
    }
}

object FreeA extends FreeAInstances with FreeAFunctions {
  case class Pure[F[+_]: Functor, +A](a: A)                            extends FreeA[F, A]
  case class Ap[F[+_]: Functor, A, +B](fa: F[A], apf: FreeA[F, A ⇒ B]) extends FreeA[F, B]
}

trait FreeAInstances {
  implicit def freeApplicative[F[+_]: Functor]: Applicative[({type λ[α] = FreeA[F, α]})#λ] =
    new Applicative[({type λ[α] = FreeA[F, α]})#λ] {
      def point[A](a: ⇒ A) = FreeA.pure(a)
      override def map[A, B](fa: FreeA[F, A])(f: A ⇒ B): FreeA[F, B] = fa map f
      def ap[A, B](fa: ⇒ FreeA[F, A])(f: ⇒ FreeA[F, A ⇒ B]): FreeA[F, B] = fa <*> f
    }
}

trait FreeAFunctions {
  import FreeA._
  def liftFreeA[F[+_]: Functor, A](x: F[A]): FreeA[F, A] = Ap[F, A, A](x, Pure(identity _))
  def pure[F[+_]: Functor, A](a: ⇒ A) = Pure[F, A](a)
}
