package workshop

import java.time.{LocalDate, MonthDay}

import io.circe.Decoder.Result
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import cats.implicits._
// import io.circe.generic.auto._ // generates automaticaly encoders and decoders for (ADT)

import scala.concurrent.duration.FiniteDuration

object circe {

  case class User(name: String)
  case class Project(maintainer: User, stars: Int)

  // Encoder allows us to encode values to a Json AST
  implicit def userEncoder: Encoder[User] = new Encoder[User] {
    def apply(u: User): Json = Json.obj(
      "name" -> Json.fromString(u.name)
    )
  }

  val userJson: String = User("jonathan").asJson.spaces2


  //Implement an encoder for `Project`
  implicit def projectEncoder: Encoder[Project] = new Encoder[Project] {
    def apply(p: Project): Json = Json.obj(
      "maintainer" -> p.maintainer.asJson,
      "stars" -> Json.fromInt(p.stars)
    )
  }



  // Decoders are used to turn a `Json` AST into a data type.
  // It can be seen as a function `Json => Either[DecodingFailure, A]`
  // circe uses an alias `Result` that is equivalent to `Either[DecodingFailure, A]
  implicit def userDecoder: Decoder[User] = new Decoder[User] {
    def apply(c: HCursor): Result[User] =
      // To go from an HCursor to a `Result`, we use the downField method to retrieve a json field
      // Then we can use the `as[A]` method to turn it into a `Result[A]`,
      // this only works when `A` has its own `Decoder` instance.
      c.downField("name").as[String].map(name => User(name))
  }


  implicit def projectDecoder: Decoder[Project] = new Decoder[Project] {
    def apply(c: HCursor): Result[Project] = for {
      stars <- c.downField("stars").as[Int]
      maintainer <- c.downField("maintainer").as[User]
    } yield Project(maintainer, stars)
  }


  val projectJson = """{ "stars" : 42, "maintainer" : { "name" : "Jonathan" } }"""

  val invalidProjectJson = """ { "moons" : 21, "stars" : "too many", "maintainer" : { "firstname": "Jonathan" } }"""

  val project = decode[Project](projectJson)

  val projectNope = decode[Project](invalidProjectJson)





  // Define an encoder and decoder instance for `Holiday`
  sealed trait HolidayType
  case class Fixed(date: String) extends HolidayType
  case class Floating(date: LocalDate) extends HolidayType

  // Define an encoder and decoder instance for `Holiday`
  case class Holiday(id: Int, name: String, holidayType: HolidayType)

  implicit def holidayTypeEncoder: Encoder[HolidayType] = new Encoder[HolidayType] {
    def apply(t: HolidayType): Json = t match {
      case Fixed(date) => date.asJson
      case Floating(date) => date.asJson
    }
  }

  implicit def holidayTypeDecoder: Decoder[HolidayType] = 
    Decoder[LocalDate].map(Floating(_)).or(Decoder[String].map(Fixed(_)))

  implicit def holidayEncoder: Encoder[Holiday] = new Encoder[Holiday] {
    def apply(h: Holiday): Json = Json.obj(
      "id" -> h.id.asJson,
      "name" -> h.name.asJson,
      "holidayType" -> h.holidayType.asJson
    )
  }

  implicit def holidayDecoder: Decoder[Holiday] = new Decoder[Holiday] {
    def apply(c: HCursor): Result[Holiday] = for {
      id <- c.downField("id").as[Int]
      name <- c.downField("name").as[String]
      holidayType <- c.downField("holidayType").as[HolidayType]
    } yield Holiday(id, name, holidayType)
  }

}
