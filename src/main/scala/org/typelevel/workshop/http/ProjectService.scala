package org.typelevel.workshop.http

import cats.effect._
import cats.implicits._
import io.circe.syntax._
import io.circe.generic.auto._
import org.http4s.HttpService
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.typelevel.workshop.algebra.ProjectRepository
import org.typelevel.workshop.algebra.Logging

class ProjectService[F[_]: Sync: ProjectRepository: Logging] extends Http4sDsl[F] {

  def service: HttpService[F] = HttpService[F] {

    case GET -> Root / name =>
      ProjectRepository[F].findByName(name).flatMap {
        case Some(project) => 
        	Logging[F].logInfo(s"Found Project: ${project.name}") *> Ok(project)
        case None => NotFound(s"No project found: $name".asJson)
      }

    case req @ DELETE -> Root / name =>
      ProjectRepository[F].deleteProject(name).flatMap(_ => NoContent())

  }
}
