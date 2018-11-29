package org.typelevel.workshop.algebra

import org.typelevel.workshop.model.Project

trait ProjectRepository[F[_]] {
  def findByName(name: String): F[Option[Project]]

  def deleteProject(name: String): F[Unit]

  def getAll: F[List[Project]]

  def updateName(name: String, newName: String): F[Option[Project]]
}

object ProjectRepository {
  def apply[F[_]: ProjectRepository]: ProjectRepository[F] = implicitly
}
