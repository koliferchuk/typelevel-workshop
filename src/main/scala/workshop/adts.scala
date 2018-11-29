package workshop

import scala.math.abs

object adts {

  // Design a data type for coffee sizes, should have small medium and large
  sealed trait Size
  case object Small extends Size
  case object Medium extends Size
  case object Large extends Size

  // Model a data type for a contact that can either be an email or a phone number
  sealed trait Contact
  case class Email(e: String) extends Contact
  case class PhoneNumber(e: String) extends Contact

  // Design a data type for a chess piece and its position on the chess board
  // type ChessPiece = Unit

  sealed trait Color 
  case object Black extends Color
  case object White extends Color

  case class Position(x: Int, y: Int)

  sealed trait ChessPiece {
    val color: Color
    val position: Position
  }
  case class King(color: Color, position: Position) extends ChessPiece
  case class Queen(color: Color, position: Position) extends ChessPiece
  case class Bishop(color: Color, position: Position) extends ChessPiece
  case class Kinght(color: Color, position: Position) extends ChessPiece
  case class Rook(color: Color, position: Position) extends ChessPiece
  case class Pawn(color: Color, position: Position) extends ChessPiece

  def move(chessPiece: ChessPiece, p: Position): Boolean = {
    val pos = chessPiece.position
    val dx = abs((pos.x - p.x))
    val dy = abs((pos.y - p.y))
    chessPiece match {
      case King(_, _) if dx == 1 && dy == 0 => true 
      case King(_, _) if dx == 1 && dy == 1 => true 
      case King(_, _) if dx == 0 && dy == 1 => true 
      case Queen(_, _) if dx == 0 || dy == 0 || dx == dy => true
      case Bishop(_, _) if dx == dy => true
      case Kinght(_, _) if dx == 1 && dy == 2 => true
      case Kinght(_, _) if dx == 2 && dy == 1 => true
      case Rook(_, _) if dx == 0 || dy == 0 => true
      case Pawn(_, _) if dy == 1 && dx == 0 => true
      case _ => false
    }
  }

  // Write a function using pattern mathcing that takes a square and returns whether it can move there or not

  // Model a data type that stores time spans
  sealed trait TimeSpan {
    val value: Int
  }
  case class Seconds(value: Int) extends TimeSpan
  case class Minutes(value: Int) extends TimeSpan
  case class Hours(value: Int) extends TimeSpan

  def add(a: TimeSpan, b: TimeSpan): TimeSpan = (a, b) match {
    case (Seconds(asec), Seconds(bsec)) => Seconds(asec + bsec)
    case (Seconds(sec), Minutes(min)) => Seconds(sec + 60 * min)
    case (Seconds(sec), Hours(hours)) => Seconds(sec + 3600 * hours)
    case (Minutes(min), Seconds(sec)) => Seconds(sec + 60 * min)
    case (Minutes(amin), Minutes(bmin)) => Minutes(amin + bmin)
    case (Minutes(min), Hours(hours)) => Minutes(min + 60 * hours)
    case (Hours(hours), Seconds(sec)) => Seconds(sec + 3600 * hours)
    case (Hours(hours), Minutes(min)) => Minutes(min + 60 * hours)
    case (Hours(ahours), Hours(bhours)) => Hours(ahours + bhours)
  }


  // Write a function that adds two TimeSpan values together

  // List all values of the type `Unit`
  def allValuesUnit: Set[Unit] = Set( () )

  // List all values of the type `Nothing`
  def allValuesNothing: Set[Nothing] = Set.empty

  // List all values of the type `Boolean`
  def allValuesBoolean: Set[Boolean] = Set(true, false)

  // List all values of the type `Size`
  def allValuesSize: Set[Size] = Set(Small, Medium, Large)

  // List all values of the type `(Size, Boolean)`
  def allValuesTuple: Set[(Size, Boolean)] = Set(
    (Small, true), (Small, false),
    (Medium, true), (Medium, false),
    (Large, true), (Large, false)
  )

  // List all values of the type `Either[Size, Boolean]`
  def allValuesEither: Set[Either[Size, Boolean]] = allValuesSize.map(Left(_)) ++ allValuesBoolean.map(Right(_))

  // List all values of the type `(Size, Unit)`
  def allValuesTupleUnit: Set[(Size, Unit)] = allValuesSize.map((_, ()))

  // List all values of the type `Either[Boolean, Nothing]`
  def allValuesEitherNothing: Set[Either[Boolean, Nothing]] = Set(Left(true), Left(false))

}
