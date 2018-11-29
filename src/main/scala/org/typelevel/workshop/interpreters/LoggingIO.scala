package org.typelevel.workshop.interpreters

import org.typelevel.workshop.algebra.Logging
import cats.effect.IO

object LoggingIO {

  implicit def logingInterpreter: Logging[IO] = new Logging[IO] {
  	def logError(e: String): IO[Unit] = IO(println(s"ERROR: $e"))
		def logInfo(i: String): IO[Unit] = IO(println(s"INFO: $i"))
  }
}