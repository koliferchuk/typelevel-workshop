package org.typelevel.workshop.algebra

trait Logging[F[_]] {
	def logError(e: String): F[Unit]
	def logInfo(i: String): F[Unit]
}

object Logging {
  def apply[F[_]: Logging]: Logging[F] = implicitly
}