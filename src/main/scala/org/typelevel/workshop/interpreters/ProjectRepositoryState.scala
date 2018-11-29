package org.typelevel.workshop.interpreters

import org.typelevel.workshop.algebra.ProjectRepository
import org.typelevel.workshop.model.Project
import cats.data.State

object ProjectRepositoryState {
  implicit def projectRepoInterpreter: ProjectRepository[State[List[Project], ?]] =
    new ProjectRepository[State[List[Project], ?]] {
      def findByName(name: String): State[List[Project], Option[Project]] =
        State.get[List[Project]].map(_.find(_.name == name))

      def deleteProject(name: String): State[List[Project], Unit] =
        State.modify[List[Project]](_.filter(_.name == name))

      def getAll: State[List[Project], List[Project]] = 
      	State.get[List[Project]]

      def updateName(oldName: String, newName: String): State[List[Project], Option[Project]] = for {
      	project <- findByName(oldName)
      	_ <- deleteProject(oldName)
      	_ <- State.modify[List[Project]](list => list ++ project.map(_.copy(name = newName)))
      } yield project
    }
}
