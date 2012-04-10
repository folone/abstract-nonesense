object Birds {

  // I combinator - identity bird / idion bird
  type Idiot = {
    type Apply[A] = A
  }
  val id: Idiot#Apply[List[Int]] = Nil

  // K combinator - kestrel - const. Corresponds to encoding of true in the lambda calculus.
  type Kestrel[A] = {
    type Apply[B] = A
  }
  val kestrel: Kestrel[List[Int]]#Apply[List[String]] = Nil

  // B combinator, function composition.
  type Bluebird[F[_], G[_]] = {
    type Apply[A] = F[G[A]]
  }
  val bluebird: Bluebird[List, List]#Apply[Int] = Nil

  // C combinator - cardinal - flip.
  type Cardinal[F[_,_]] = {
    type Apply[A,B] = F[B,A]
  }
  val cardinal: Cardinal[Map]#Apply[String, Int] = Map.empty

  // A combinator - apply/applicator, ($)
  type Applicator[F[_]] = {
    type Apply[A] = F[A]
  }
  val applicator: Applicator[List]#Apply[Int] = Nil

  // Psi combinator- psi bird, on.
  type Psi[P[_,_], F[_]] = { 
    type Apply[A, B] = P[F[A], F[B]]
  }
  val psi: Psi[Map, List]#Apply[Int, String] = Map.empty

  // B3 combinator - becard.
  type Becard[F[_], G[_], H[_]] = {
    type Apply[A] = F[G[H[A]]]
  }
  val becard: Becard[List,Set,List]#Apply[Int] = Nil

  // B1 combinator - blackbird
  type Blackbird[F[_], G[_,_]] = {
    type Apply[A,B] = F[G[A,B]]
  }
  val blackbird: Blackbird[List,Either]#Apply[Int,String] = Nil

  // B' combinator - bluebird prime.
  type BluebirdPrime[F[_,_],G[_]] = {
    type Apply[A,B] = F[A,G[B]]
  }
  val bluebirdPrime: BluebirdPrime[Map,List]#Apply[Int,String] = Map.empty

  //  B2 combinator - bunting.
  type Bunting[F[_], G[_,_,_]] = {
    type Apply[A,B,C] = F[G[A,B,C]]
  }
  import scala.collection.generic._
  val bunting: Bunting[List, CanBuildFrom]#Apply[List[String], Int, Set[String]] = Nil

  // C' combinator - no name.
  type CardinalPrime[F[_,_], G[_]] = {
    type Apply[A,B] = F[G[B],A]
  }
  val cardinalPrime: CardinalPrime[Map, List]#Apply[Int, String] = Map.empty

  // C* combinator - cardinal once removed.
  type CardinalStar[F[_,_,_]] = {
    type Apply[A,B,C] = F[A,C,B]
  }
  var cardinalStar: CardinalStar[CanBuildFrom]#Apply[List[String], Int, Set[String]] = _

  // C** combinator - cardinal twice removed.
  type CardinalStarStar[F[_,_,_,_]] = {
    type Apply[A,B,C,D] = F[A,B,D,C]
  }
  // TODO example

  // D1 combinator - dickcissel.
  type Dickcissel[F[_,_,_], G[_]] = {
    type Apply[A,B,C] = F[A,B,G[C]]
  }
  // TODO example

  // D combinator - dove.
  type Dove[F[_,_], G[_]] = {
    type Apply[A,B] = F[A,G[B]]
  }
  // TODO example

  // D2 combinator - dovekie.
  type Dovekie[F[_,_], G[_], H[_]] = {
    type Apply[A,B] = F[G[A],H[B]]
  }
  // TODO example

  // E combinator - eagle.
  type Eagle[F[_,_], G[_,_]] = {
    type Apply[A,B,C] = F[A,G[B,C]]
  }
  // TODO example

  // E Combinator - bald eagle.
  // For alphabetical regularity it is somewhat misnamed here as eaglebald.
  type Eaglebald[F[_,_], G[_,_], H[_,_]] = {
    type Apply[A,B,C,D] = F[G[A,B],H[C,D]]
  }

  // M combinator - mockingbird (Really?).
  type Mockingbird[F[_]] = {
    type Apply = F[F[_]]
  }
  val p: Mockingbird[List]#Apply = Nil

}
