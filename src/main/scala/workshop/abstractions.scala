package workshop

import workshop.typeclasses._
import workshop.model._
import simulacrum.typeclass
import scala.concurrent.Future
import abstractions.Monoidal.ops._
import workshop.typeclasses.Monoid.ops._
import abstractions.Traverse.ops._
import abstractions.Foldable.ops._
import scala.concurrent.ExecutionContext.Implicits.global


object abstractions {


  //Multiplicative Monoidal Functors

  @typeclass trait Monoidal[F[_]] extends Functor[F] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

    def unit: F[Unit]

    def pure[A](a: A): F[A] = map(unit)(_ => a)

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      map(product(fa, fb)) { case (a, b) => f(a, b) }
  }

  implicit def optionMonoidal: Monoidal[Option] = new Monoidal[Option] {
    def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
      case (Some(a), Some(b)) => Some((a, b))
      case _ => None 
    }
    def unit: Option[Unit] = Some(())

    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def futureMonoidal: Monoidal[Future] = new Monoidal[Future] {
    def unit: Future[Unit] = Future.successful(())
    def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
    def product[A, B](fa: Future[A], fb: Future[B]): Future[(A, B)] = for {
      a <- fa
      b <- fb
    } yield (a, b)
  }

  // There are two possible solutions here, can you figure out which?
  implicit def listMonoidal: Monoidal[List] = new Monoidal[List] {
    def unit: List[Unit] = List(())
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = for {
      a <- fa
      b <- fb
    } yield (a, b)
  }


  def bundle[F[_]: Monoidal, A](x: F[A], y: F[A]): F[List[A]] = Monoidal[F].map2(x, y)((a, b) => List(a, b)) //x.product(y).map{ case (a, b) => List(a, b) }

  def appendM[F[_]: Monoidal, A](x: F[A], y: F[List[A]]): F[List[A]] = Monoidal[F].map2(x, y)((a: A, list: List[A]) => a :: list)

  def sequence[F[_]: Monoidal, A](list: List[F[A]]): F[List[A]] = list.foldLeft(Monoidal[F].pure(List.empty[A])) { (acc: F[List[A]], cur: F[A]) =>
    appendM(cur, acc)
  }

  def traverse[F[_]: Monoidal, A, B](list: List[A])(f: A => F[B]): F[List[B]] = list.foldLeft(Monoidal[F].pure(List.empty[B])){ (acc: F[List[B]], cur: A) =>
    appendM(f(cur), acc)
  }

  def ap[F[_]: Monoidal, A, B](ff: F[A => B], fa: F[A]): F[B] = Monoidal[F].map2(ff, fa){(f: A => B, a: A) => f(a)}

  //Given two Option[Int] multiply the int values if they exist or leave them unchanged if one of them doesn't
  def combineOptions(x: Option[Int], y: Option[Int]): Option[Int] = Monoidal[Option].map2(x, y)((a, b) => a * b)

  //Foldable

  @typeclass trait Foldable[F[_]] {
    def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

    def combineAll[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)
  }

  implicit def optionFoldable: Foldable[Option] = new Foldable[Option] {
    def foldMap[A, B: Monoid](fa: Option[A])(f: A => B): B = fa.foldLeft(Monoid[B].empty) {(b, a) => b |+| f(a)}
  }

  implicit def listFoldable: Foldable[List] = new Foldable[List] {
    def foldMap[A, B: Monoid](fa: List[A])(f: A => B): B = fa.foldLeft(Monoid[B].empty){(b, a) => b |+| f(a)}
  }

  implicit def setFoldable: Foldable[Set] = new Foldable[Set] {
    def foldMap[A, B: Monoid](fa: Set[A])(f: A => B): B = fa.foldLeft(Monoid[B].empty){(b, a) => b |+| f(a)}
  }

  // Turn this foldable into a List
  def fromFoldable[F[_]: Foldable, A](fa: F[A]): List[A] = fa.foldMap(a => List(a))

  // Find the first element that matches the predicate
  // Hint: YOu might need to defne a new type with a new monoid
  def find[F[_]: Foldable, A](fa: F[A], f: A => Boolean): Option[A] = {
    case class FindMonoid(value: Option[A])

    implicit def findMonoid: Monoid[FindMonoid] = new Monoid[FindMonoid] {
      def empty: FindMonoid = FindMonoid(None)
      def combine(x: FindMonoid, y: FindMonoid): FindMonoid = (x.value, y.value) match {
        case (Some(a), _) => FindMonoid(Some(a))
        case (None, Some(a)) => FindMonoid(Some(a))
        case _ => FindMonoid(None)
      }
    }

    Foldable[F].foldMap(fa)(a => if(f(a)) FindMonoid(Some(a)) else FindMonoid(None)).value
  }

  //Traversable
  @typeclass trait Traverse[F[_]]  {
    def traverse[G[_]: Monoidal, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

    def sequence[G[_]: Monoidal, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)
  }

  implicit def listTraversable: Traverse[List] = new Traverse[List] {
    def traverse[G[_]: Monoidal, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = 
      fa.foldLeft(Monoidal[G].pure(List.empty[B])){ (acc: G[List[B]], cur: A) =>
        appendM(f(cur), acc)
    }
  }

  implicit def optionTraversable: Traverse[Option] = new Traverse[Option] {
    def traverse[G[_]: Monoidal, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = fa match {
      case Some(a) => f(a).map(Some(_))
      case None => Monoidal[G].pure(None)
    }
  }

  // kind Projector !!!
  implicit def eitherTraversable[E]: Traverse[Either[E, ?]] = new Traverse[Either[E, ?]] {
    def traverse[G[_]: Monoidal, A, B](fa: Either[E, A])(f: A => G[B]): G[Either[E, B]] = fa match {
      case Right(a) => f(a).map(Right(_))
      case Left(e) => Monoidal[G].pure(Left(e))
    }
  }

  //Validated

  sealed trait Validated[+E, +A]
  case class Valid[+A](a: A) extends Validated[Nothing, A]
  case class Invalid[+E](e: E) extends Validated[E, Nothing]

  type ValidatedList[+E, +A] = Validated[List[E], A]

  def toEither[E, A](v: Validated[E, A]): Either[E, A] = v match {
    case Valid(a) => Right(a)
    case Invalid(e) => Left(e)
  }

  def toValidated[E, A](e: Either[E, A]): Validated[E, A] = e match {
    case Right(a) => Valid(a)
    case Left(e) => Invalid(e)
  }

  implicit def validatedMonoidal[E: Monoid]: Monoidal[Validated[E, ?]] = new Monoidal[Validated[E, ?]] {
    def product[A, B](fa: Validated[E, A], fb: Validated[E, B]): Validated[E, (A, B)] = (fa, fb) match {
      case (Valid(a), Valid(b)) => Valid((a, b))
      case (Invalid(e1), Invalid(e2)) => Invalid(e1 |+| e2)
      case (_, Invalid(e)) => Invalid(e)
      case (Invalid(e), _) => Invalid(e)
    }

    def unit: Validated[E, Unit] = Valid(())

    def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] = fa match {
      case Valid(a) => Valid(f(a))
      case Invalid(e) => Invalid(e)
    } 
  }

  implicit def validatedTraversable[E]: Traverse[Validated[E, ?]] = new Traverse[Validated[E, ?]] {
    def traverse[G[_]: Monoidal, A, B](fa: Validated[E, A])(f: A => G[B]): G[Validated[E, B]] = fa match {
      case Valid(a) => f(a).map(Valid(_))
      case Invalid(e) => Monoidal[G].pure(Invalid(e))
    }
  }


  // Validation exercise
  // Use `Validated` and everything you've learned so far to solve this one
  // In the `model` object you can find two lists of unvalidated strings representing users
  // Your job is to check all of them whether they are valid or not.
  // To do so, you should use the `User.validate` function.
  // Once your done, you can check the difference to Either
  def allUsers(list: List[String]): ValidatedList[String, List[User]] = list.traverse(User.validate)





  // Next we want to write a function that takes a String representing a user
  // and return the UserReport for that user using the `User.fetchReport` function
  // def fetchReport(u: User): Future[UserReport]
  // def validate(u: String): ValidatedList[String, User]
  def reportForUser(u: String): Future[ValidatedList[String, UserReport]] = 
    User.validate(u).traverse(User.fetchReport)




  // Hard: Now get all reports for all the users
  // type ValidatedList[+E, +A] = Validated[List[E], A]
  // Future[Validated[List[String], List[UserReport]]]
  // Future[ValidatedList[String, UserReport]]
  def allReports(list: List[String]): Future[ValidatedList[String, List[UserReport]]] = 
    list.traverse(reportForUser).map(_.sequence)


  // Nested Monoidals
  // extendes AnyVal (value classes) !!!

  case class Nested[F[_], G[_], A](value: F[G[A]])

  implicit def nestedMonoidal[F[_]: Monoidal, G[_]: Monoidal]: Monoidal[Nested[F, G, ?]] = new Monoidal[Nested[F, G, ?]] {
    def product[A, B](fa: Nested[F, G, A], fb: Nested[F, G, B]): Nested[F, G, (A, B)] = {
      val fga: F[G[A]] = fa.value
      val fgb: F[G[B]] = fb.value

      val tupleF: F[(G[A], G[B])] = fga.product(fgb)

      Nested(tupleF.map{ tuple => 
        val ga: G[A] = tuple._1
        val gb: G[B] = tuple._2

        ga.product(gb)
      })
    }

    def unit: Nested[F, G, Unit] = Nested(Monoidal[F].pure(Monoidal[G].unit))

    def map[A, B](fa: Nested[F, G, A])(f: A => B): Nested[F, G, B] = Nested(fa.value.map(_.map(f)))
  }

  // Try implementing `allReports` using `Nested`, it should be much easier this way
  def allReportsUsingNested(list: List[String]): Future[ValidatedList[String, List[UserReport]]] = 
    list.traverse(e => Nested(reportForUser(e))).value


  @typeclass trait ContravariantFunctor[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }

  case class Predicate[A](run: A => Boolean)

  case class StringEncoder[A](run: A => String)

  implicit def predicateContravariant: ContravariantFunctor[Predicate] = new ContravariantFunctor[Predicate] {
    def contramap[A, B](fa: Predicate[A])(f: B => A): Predicate[B] = Predicate(f andThen fa.run)
  }

  implicit def stringEncoderContravariant: ContravariantFunctor[StringEncoder] = new ContravariantFunctor[StringEncoder] {
    def contramap[A, B](fa: StringEncoder[A])(f: B => A): StringEncoder[B] = StringEncoder(f andThen fa.run)
  }
}
