package workshop

import com.sun.xml.internal.ws.policy.sourcemodel.ModelNode
import simulacrum.typeclass
import workshop.typeclasses._
import workshop.abstractions.Monoidal
import workshop.typeclasses.Monoid.ops._
import workshop.monoids.Category.ops._
import abstractions.Traverse.ops._
import workshop.monoids.Profunctor.ops._
import workshop.monoids.Monad.ops._
import workshop.util._

import scala.util.Try
import scala.concurrent.Future
import scala.io.StdIn
import java.io.File

object monoids {

  // Additive Monoidal

  @typeclass trait AddMonoidal[F[_]] extends Functor[F] {
    def sum[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]

    def zero[A]: F[A]

    def combineK[A](x: F[A], y: F[A]): F[A] =
      map(sum(x, y))(_.merge)
  }

  // Category

  @typeclass trait Category[F[_, _]] {
    def compose[A, B, C](fab: F[A, B], fbc: F[B, C]): F[A, C]

    def identity[A]: F[A, A]

    def >>>[A, B, C](fab: F[A, B], fbc: F[B, C]): F[A, C] = compose(fab, fbc)

    def <<<[A, B, C](fbc: F[B, C], fab: F[A, B]): F[A, C] = compose(fab, fbc)
  }

  implicit def categoryFunction: Category[Function1] = new Category[Function1] {
    def compose[A, B, C](fab: A => B, fbc: B => C): A => C = fab andThen fbc

    def identity[A]: A => A = a => a
  }

  implicit def monoidEndoCategory[F[_, _]: Category, A]: Monoid[F[A, A]] = new Monoid[F[A, A]] {
    def empty: F[A, A] = Category[F].identity

    def combine(x: F[A, A], y: F[A, A]): F[A, A] = x >>> y
  }

  def plusOne: Int => Int = _ + 1

  def times3: Int => Int = _ * 3

  def plusOneTimes3: Int => Int = plusOne |+| times3


  def plusOneTimes3ToString: Int => String = plusOneTimes3 >>> (_.toString)


  // Different Category instances
  case class OptionFunction[A, B](apply: A => Option[B])

  case class EffectFunction[A, B](apply: A => B)


  implicit def categoryOptionFunction: Category[OptionFunction] = new Category[OptionFunction] {
    def identity[A]: OptionFunction[A, A] = OptionFunction(a => Some(a))

    def compose[A, B, C](fab: OptionFunction[A, B], fbc: OptionFunction[B, C]): OptionFunction[A, C] =
      OptionFunction { a =>
        fab.apply(a) match {
          case Some(b) => fbc.apply(b)
          case None => None
        }
      }
  }

  implicit def categoryEffectFunction: Category[EffectFunction] = new Category[EffectFunction] {
    def compose[A, B, C](fab: EffectFunction[A, B], fbc: EffectFunction[B, C]): EffectFunction[A, C] = 
      EffectFunction(fab.apply andThen fbc.apply)

    def identity[A]: EffectFunction[A, A] = EffectFunction(a => a)
  }


  // We can define real life synchronous programs without breaking referential transparency using EffectFunction

  trait Program {
    def program: EffectFunction[List[String], Unit]

    def main(args: Array[String]): Unit =
      program.apply(args.toList)
  }

  // F[_, _] example: Function1
  // lmap maps input; rmap maps output

  @typeclass trait Profunctor[F[_, _]] {
    def dimap[A, B, C, D](fac: F[B, C])(f: A => B)(g: C => D): F[A, D]

    def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] = dimap(fab)(identity[A])(f)

    def lmap[A, B, C](fbc: F[B, C])(f: A => B): F[A, C] = dimap(fbc)(f)(identity)
  }

  implicit def profunctorFunction: Profunctor[Function1] = new Profunctor[Function1] {
    def dimap[A, B, C, D](fac: B => C)(f: A => B)(g: C => D): A => D =
      f >>> fac >>> g
  }

  implicit def profunctorEffectFunction: Profunctor[EffectFunction] = new Profunctor[EffectFunction] {
    def dimap[A, B, C, D](fac: EffectFunction[B, C])(f: A => B)(g: C => D): EffectFunction[A, D] = 
      EffectFunction(f >>> fac.apply >>> g)
  }

  implicit def profunctorOptionFunction: Profunctor[OptionFunction] = new Profunctor[OptionFunction] {
    def dimap[A, B, C, D](fac: OptionFunction[B, C])(f: A => B)(g: C => D): OptionFunction[A, D] = 
      OptionFunction(f >>> fac.apply >>> (_.map(g)))
  }



  // Now try to define an EffectFunction that prompts you to type your name,
  // then reads your name from stdin and outputs a greeting with your name.
  // To do so, you can use the `readLine` and `printLine` functions from `util`.
  // def readLine: EffectFunction[Unit, String]
  // def printLine: EffectFunction[String, Unit]
  def consoleProgram: EffectFunction[Unit, Unit] = 
    util.printLine.lmap((_: Unit) => "Please write your name: ") >>> util.readLine >>> util.printLine.lmap(name => s"Hello $name")


  // We can define functions that might fail with a value

  case class FailFunction[A, B](apply: A => Either[Throwable, B])

  implicit def categoryFailFunction: Category[FailFunction] = new Category[FailFunction] {
    def compose[A, B, C](fab: FailFunction[A, B], fbc: FailFunction[B, C]): FailFunction[A, C] = 
      FailFunction { a => 
        fab.apply(a) match {
          case Right(b) => fbc.apply(b)
          case Left(e) => Left(e)
        }
      }

    def identity[A]: FailFunction[A, A] = FailFunction(a => Right(a))
  }

  implicit def profunctorFailFunction: Profunctor[FailFunction] = new Profunctor[FailFunction] {
    def dimap[A, B, C, D](fac: FailFunction[B, C])(f: A => B)(g: C => D): FailFunction[A, D] = 
      FailFunction(a => fac.apply(f(a)) match {
        case Right(c) => Right(g(c))
        case Left(e) => Left(e)
      })
  }


  trait FailProgram {
    def program: FailFunction[List[String], Unit]

    def main(args: Array[String]): Unit =
      program.apply(args.toList) match {
        case Left(t) => throw t
        case _ => ()
      }
  }

  // Next try to define a FailFunction that reads a file name from stdin, then reads from that file and prints out its content
  // You can try using the `data/test.txt` file.
  // def readFile: FailFunction[File, String] 

  def convert[A, B](ef: EffectFunction[A, B]): FailFunction[A, B] = FailFunction(a => Right(ef.apply(a)))

  def fileProgram: FailFunction[Unit, Unit] = convert(util.readLine) >>> util.readFile.lmap(new File(_)) >>> convert(util.printLine)

  // Tasks
  // case class Task[A](() => Either[Throwable, B])

  type Task[A] = FailFunction[Unit, A]

  def newCompose[A, B](ta: Task[A])(f: FailFunction[A, B]): Task[B] = ta >>> f


  type OptionTask[A] = OptionFunction[Unit, A]

  def optionCompose[A, B](ta: OptionTask[A])(f: OptionFunction[A, B]): OptionTask[B] = ta >>> f




  // Monad

  @typeclass trait Monad[F[_]] extends Monoidal[F] {
    // composing effectful functions, where effect could be an Option or Either or sth. else 
    def flatMap[A, B](fa: /* Unit => */ F[A])(f: A => F[B]): /* Unit => */ F[B]

    def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      flatMap(fa)(a => map(fb)(b => (a, b)))

    override def map[A, B](fa: F[A])(f: A => B): F[B] = {
      flatMap(fa)(a => pure(f(a)))
    }
  }

  implicit def monadOption: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case None => None
    }

    def unit: Option[Unit] = Some(())
  }

  // case class FailFunction[A, B](apply: A => Either[Throwable, B])
  // type Task[A] = FailFunction[Unit, A]
  
  implicit def monadTask: Monad[Task] = new Monad[Task] {
    def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] = fa.apply(()) match {
      case Right(a) => f(a)
      case Left(e) => FailFunction(_ => Left(e))
    }

    def unit: Task[Unit] = FailFunction((_: Unit) => Right(()))
  }

  implicit def monadEither[E]: Monad[Either[E, ?]] = new Monad[Either[E, ?]] {
    def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

    def unit: Either[E, Unit] = Right(())
  }


  def composeMonadFunctions[F[_]: Monad, A, B, C](x: A => F[B], y: B => F[C]): A => F[C] = 
    a => x(a).flatMap(y(_))



  // Kleisli
  // Kleisli is a function which takes a value, applies a function and returns a value wraped in the some context (Option, Either, Future,...)
  case class Kleisli[F[_], A, B](apply: A => F[B])

  implicit def categoryKleisli[F[_]: Monad]: Category[Kleisli[F, ?, ?]] = new Category[Kleisli[F, ?, ?]] {
    def compose[A, B, C](fab: Kleisli[F, A, B], fbc: Kleisli[F, B, C]): Kleisli[F, A, C] = 
      Kleisli(a => fab.apply(a).flatMap(fbc.apply))

    def identity[A]: Kleisli[F, A, A] = Kleisli(Monad[F].pure)
  }

  implicit def profunctorKleisli[F[_]: Monad]: Profunctor[Kleisli[F, ?, ?]] = new Profunctor[Kleisli[F, ?, ?]] {
    def dimap[A, B, C, D](fac: Kleisli[F, B, C])(f: A => B)(g: C => D): Kleisli[F, A, D] = 
      Kleisli(a => fac.apply(f(a)).map(g))
  }


  // Now that we have Kleisli, go back and redefine OptionFunction and FailFunction as a special case of Kleisli

  type OptionFunc[A, B] = Kleisli[Option, A, B]
  type FailFunc[A, B] = Kleisli[Either[Throwable, ?], A, B]
  // IO

  case class IO[A](unsafeRun: () => A)

  implicit def monadIO: Monad[IO] = new Monad[IO] {
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = 
      IO(() => f(fa.unsafeRun()).unsafeRun()) // f(fa.unsafeRun()) - will compile but is UNSAFE!!! Running unsafeRun function is alowed only in main method and inside IO ( IO(() => unsafe call) )

    def unit: IO[Unit] = IO(() => ())

    override def map[A, B](fa: IO[A])(f: A => B): IO[B] = 
      IO(() => f(fa.unsafeRun()))
  }

  // Run both effects one after another, but only return the result of the second (IO[Int] *> IO[String] == IO[String])
  def ignoreFirstResult[A, B](fa: IO[A], fb: IO[B]): IO[B] = 
    fa.product(fb).map { case (_, b) => b }

  // Run both effects one after another, but only return the result of the first (IO[Int] <* IO[String] == IO[Int])
  def ignoreSecondResult[A, B](fa: IO[A], fb: IO[B]): IO[A] = 
    fa.product(fb).map { case (a, _) => a }

  // Reimplement fileprogram using `IO` instead
  // Tip: You can use for-comprehensions, you can try writing a version with and without using for-comprehensions

  def readFile(f: File): IO[String] =
    IO(() => util.readFile.apply(f) match {
      case Right(s) => s
      case Left(e) => throw e
    })

  def readLine: IO[String] = IO(() => util.readLine.apply(()))

  def printLine(s: String): IO[Unit] = IO(() => util.printLine.apply(s))

  def fileProgramIO = for {
    file <- readLine
    text <- readFile(new File(file))
    _ <- printLine(text)
  } yield ()


  // Use IO to print out each of the names given to this function
  // You can test this using `model.userList1`
  def printAll(names: List[String]): IO[Unit] = names.traverse(printLine(_)).map(_ => ())

}
