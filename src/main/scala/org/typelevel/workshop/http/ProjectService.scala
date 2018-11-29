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

	case class UpdateNameReq(name: String)

  def service: HttpService[F] = HttpService[F] {

  	case GET -> Root => 
  		ProjectRepository[F].getAll.flatMap(res => Ok(res.asJson))

    case GET -> Root / name =>
      ProjectRepository[F].findByName(name).flatMap {
        case Some(project) => 
        	Logging[F].logInfo(s"Found Project: ${project.name}") *> Ok(project)
        case None => NotFound(s"No project found: $name".asJson)
    	}

    case req @ PUT -> Root / name => for {
	      updateName <- req.as[UpdateNameReq]
	      projectOption <- ProjectRepository[F].updateName(name, updateName.name)
	      result <- projectOption match {
	        case Some(project) => Created(project)
	        case None => NotFound(s"No project found: $name".asJson)
	      }
	    } yield result
    	

    case req @ DELETE -> Root / name =>
      ProjectRepository[F].deleteProject(name).flatMap(_ => NoContent())

  }
}
