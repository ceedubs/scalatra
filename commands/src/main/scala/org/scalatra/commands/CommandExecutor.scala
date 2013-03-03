package org.scalatra
package commands

import scala.util.control.Exception.allCatch
import grizzled.slf4j.Logger
import org.scalatra.validation
import validation._
import scalaz._
import Scalaz._

trait CommandExecutor {
  def execute[A, C <: Command](command: => C)(handle: C => ModelValidation[A]): ModelValidation[A]
}

class ExceptionCatchingCommandExecutor extends CommandExecutor {
  @transient protected[this] val commandLogger: Logger = Logger[this.type]

  override def execute[A, C <: Command](command: => C)(handle: C => ModelValidation[A]): ModelValidation[A] = {
    commandLogger.debug("Executing [%s].\n%s" format (command.getClass.getName, command))
    if (command.isValid) {
      val res = (allCatch withApply(serverError[A](command.getClass.getName, _))) {
        handle(command)
      }

      val resultLog = res.fold(
      { failures => "with %d failures\n%s".format(failures.tail.size + 1, failures.list) },
      { _ => "successfully" }
      )
      commandLogger.debug("Command [%s] executed %s." format (command.getClass.getName, resultLog))
      res
    } else {
      val f = command.errors.map(_.validation) collect {
        case Failure(e) => e
      }
      commandLogger.debug("Command [%s] executed with %d failures.\n%s" format (command.getClass.getName, f.size, f.toList))
      NonEmptyList(f.head, f.tail: _*).fail[A]
    }
  }

  protected def serverError[A](cmdName: String, ex: Throwable): ModelValidation[A] = {
    commandLogger.error("There was an error while executing " + cmdName, ex)
    ValidationError("An error occurred while handling: " + cmdName, UnknownError).failNel[A]
  }

}