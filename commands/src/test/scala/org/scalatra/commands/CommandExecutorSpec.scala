package org.scalatra
package commands

import org.specs2.mutable.Specification
import org.scalatra.util.ParamsValueReaderProperties._
import validation._
import scalaz._
import Scalaz._

class SimpleCommand extends ParamsOnlyCommand {
  val name: Field[String] = asString("name").notBlank
}


class CommandExecutorSpec extends Specification {
  implicit def ValidationErrorEqual: Equal[ValidationError] = equalA

  "The command executor" should {
    "wrap the result of a successful execution in a Success" in {
      val cmd = new SimpleCommand
      cmd.bindTo(Map("name" -> "jimmy"))
      val executor = new ExceptionCatchingCommandExecutor
      val res: ModelValidation[Option[String]] = executor.execute(cmd){ c =>
        c.name.value.success
      }
      res must_== Some("jimmy").success
    }

    "create a Failure upon validation error" in {
      val cmd = new SimpleCommand
      cmd.bindTo(Map("name" -> ""))
      val executor = new ExceptionCatchingCommandExecutor
      val res: ModelValidation[Option[String]] = executor.execute(cmd){ c =>
        c.name.value.success
      }
      val expected = ValidationError("Name is required.", FieldName("name"), ValidationFail).failNel
      res ≟ expected must beTrue
    }

    "convert an exception into a Failure" in {
      val cmd = new SimpleCommand
      cmd.bindTo(Map("name" -> "timmy"))
      val executor = new ExceptionCatchingCommandExecutor
      val res: ModelValidation[Option[String]] = executor.execute(cmd){ c =>
        throw new RuntimeException("I'm an exception")
      }
      val expected = ValidationError("An error occurred while handling: org.scalatra.commands.SimpleCommand", UnknownError).failNel
      res ≟ expected must beTrue

    }
  }
}
