package org.polystat.odin.sandbox.inlining

import cats.effect.{ExitCode, IO, IOApp}
import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.{
  EOAnonExpr,
  EOAnyNameBnd,
  EOBnd,
  EOBndExpr,
  EOCopy,
  EODot,
  EOExpr,
  EOObj,
  EOProg,
  EOSimpleApp,
  LazyName
}
import org.polystat.odin.parser.EoParser
import pprint.PPrinter
import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.backend.eolang.inlineorlines.ops._

object Inliner extends IOApp {

  def readFile(fileName: String): String = {
    val source = io.Source.fromResource(fileName)
    try source.mkString
    finally source.close()
  }

  val astPrinter: PPrinter = pprint.copy(
    additionalHandlers = { case nonEmpty: NonEmpty[_, _] =>
      pprint.treeify(nonEmpty.value)
    }
  )

  def findBndByName(
    bnds: Vector[EOBnd[Fix[EOExpr]]],
    methodName: String
  ): Option[EOBnd[Fix[EOExpr]]] = {
    bnds.find {
      case EOBndExpr(EOAnyNameBnd(LazyName(name)), _) => methodName == name
      case _ => false
    }
  }

  def findCheckMana(code: EOProg[Fix[EOExpr]]): Option[EOExpr[Fix[EOExpr]]] = {
    val character: Option[EOBnd[Fix[EOExpr]]] =
      findBndByName(code.bnds, "character")

    character.flatMap {
      case EOAnonExpr(_) => None
      case EOBndExpr(_, expr) => Fix.un(expr) match {
          case EOObj(_, _, bndAttrs) =>
            findBndByName(bndAttrs, "checkMana") match {
              case Some(value) =>
                Some(Fix.un(value.expr))
              case None => None
            }
          case _ => None
        }
    }
  }

  def findCheckManaCalls(
    code: EOProg[Fix[EOExpr]]
  ): List[EOCopy[Fix[EOExpr]]] = {
    def exprHelper(expr: EOExpr[Fix[EOExpr]]): List[EOCopy[Fix[EOExpr]]] =
      expr match {
        case EOObj(_, _, bndAttrs) => bndAttrs.flatMap(bndHelper).toList
        case copy @ EOCopy(trg, args) => trg match {
            case EODot(EOSimpleApp("self"), "checkMana") =>
              List(copy) ++ args.flatMap(bndHelper).toList
            case _ => args.flatMap(bndHelper).toList
          }

        case _ => List()
      }

    def bndHelper(bnd: EOBnd[Fix[EOExpr]]): List[EOCopy[Fix[EOExpr]]] =
      bnd match {
        case EOAnonExpr(expr) => exprHelper(Fix.un(expr))
        case EOBndExpr(_, expr) => exprHelper(Fix.un(expr))
      }

    code.bnds.flatMap(bndHelper).toList
  }

  def inlineCalls(
    code: EOProg[Fix[EOExpr]],
    targets: List[EOCopy[Fix[EOExpr]]],
    replacement: EOExpr[Fix[EOExpr]]
  ): EOProg[Fix[EOExpr]] = {
    def exprHelper(expr: EOExpr[Fix[EOExpr]]): Fix[EOExpr] = expr match {
      case EOObj(freeAttrs, varargAttr, bndAttrs) =>
        Fix(EOObj(freeAttrs, varargAttr, bndAttrs.map(eoBndHelper)))
      case copy @ EOCopy(trg, args) =>
        if (targets.contains(copy)) {
          Fix(replacement)
        } else {
          Fix(EOCopy(trg, args.map(bndHelper)))
        }

      case id => Fix(id)
    }

    def eoBndHelper(bnd: EOBndExpr[Fix[EOExpr]]): EOBndExpr[Fix[EOExpr]] =
      EOBndExpr(bnd.bndName, exprHelper(Fix.un(bnd.expr)))

    def bndHelper(bnd: EOBnd[Fix[EOExpr]]): EOBnd[Fix[EOExpr]] = bnd match {
      case EOAnonExpr(expr) => EOAnonExpr(exprHelper(Fix.un(expr)))
      case EOBndExpr(tmp, expr) => EOBndExpr(tmp, exprHelper(Fix.un(expr)))
    }

    EOProg(code.metas, code.bnds.map(bndHelper))
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val sourcecode = readFile("mana.eo")
    for {
      // Task 1
      _ <- IO.println("-" * 100 ++ "Task1")
      code <- EoParser.sourceCodeEoParser[IO](2).parse(sourcecode)
      checkManaMethod <- IO.fromOption(findCheckMana(code))(
        new Exception("There is no checkMana method!!!")
      )
      _ <- IO.println(checkManaMethod.toEO.allLinesToString)

      // Task 2
      _ <- IO.println("-" * 100 ++ "Task2")
      targets = findCheckManaCalls(code)
      _ <- IO.delay(astPrinter.pprintln(targets))

      // Task 3
      _ <- IO.println("-" * 100 ++ "Task3")
      checkManaBody <- checkManaMethod match {
        case EOObj(_, _, bndAttrs) => IO.fromOption(
            bndAttrs.headOption.map(bnd => Fix.un(bnd.expr))
          )(new Exception("Method body is empty!!!!"))
        case _ =>
          IO.raiseError(new Exception("Method checkMana has no body!!!!!"))
      }
      inlinedCheckManaMethod = inlineCalls(code, targets, checkManaBody)
      _ <- IO.println(inlinedCheckManaMethod.toEO.allLinesToString)
    } yield ExitCode.Success
  }

}
