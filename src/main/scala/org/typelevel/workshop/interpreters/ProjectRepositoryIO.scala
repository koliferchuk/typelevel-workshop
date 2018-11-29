package org.typelevel.workshop.interpreters

import org.typelevel.workshop.algebra.ProjectRepository
import org.typelevel.workshop.model.Project
import cats.effect.IO
import cats.implicits._
import doobie.implicits._
import org.typelevel.workshop.db.Database

object ProjectRepositoryIO {

  implicit def projectRepoInterpreter: ProjectRepository[IO] = new ProjectRepository[IO] {
    def findByName(name: String): IO[Option[Project]] =
      sql"""
        SELECT p.id, p.name, p.description, u.id, u.username, u.email
        FROM project p JOIN user u ON p.owner = u.id
        WHERE p.name = $name
      """.query[Project].option.transact(Database.xa)


    def deleteProject(name: String): IO[Unit] = (for {
      projectId <- sql"SELECT id FROM project WHERE name = $name".query[Int].unique
      _ <- sql"DELETE FROM project WHERE id = $projectId".update.run
    } yield ()).transact(Database.xa).attempt.void

    def getAll: IO[List[Project]] = 
      sql"""
        SELECT p.id, p.name, p.description, u.id, u.username, u.email
        FROM project p JOIN user u ON p.owner = u.id
      """.query[Project].to[List].transact(Database.xa)

    def updateName(name: String, newName: String): IO[Option[Project]] = (for {
      projectId <- sql"SELECT id FROM project WHERE name = $name".query[Int].unique
      _ <- sql"""UPDATE project
        SET name = $newName
        WHERE id = $projectId""".update.run
      project <- sql"""
        SELECT p.id, p.name, p.description, u.id, u.username, u.email
        FROM project p JOIN user u ON p.owner = u.id
        WHERE p.id = $projectId""".query[Project].option
    } yield project).transact(Database.xa)
  }
}
