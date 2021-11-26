package org.polystat.odin.sandbox.inlining

import cats.effect.{ExitCode, IO, IOApp}
import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.{
  EOAnonExpr,
  EOAnyNameBnd,
  EOApp,
  EOBnd,
  EOBndExpr,
  EOCopy,
  EOData,
  EODecoration,
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

  def findCheckMana(code: EOProg[Fix[EOExpr]]): Option[EOObj[Fix[EOExpr]]] = {
    val character: Option[EOBnd[Fix[EOExpr]]] =
      findBndByName(code.bnds, "character")

    character.flatMap {
      case EOAnonExpr(_) => None
      case EOBndExpr(_, Fix(expr)) => expr match {
          case EOObj(_, _, bndAttrs) =>
            findBndByName(bndAttrs, "checkMana") match {
              case Some(EOBndExpr(_, Fix(obj: EOObj[Fix[EOExpr]]))) =>
                Some(obj)
              case _ => None
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
        case EOAnonExpr(Fix(expr)) => exprHelper(expr)
        case EOBndExpr(_, Fix(expr)) => exprHelper(expr)
      }

    code.bnds.flatMap(bndHelper).toList
  }

  def propagateParams(
    methodBody: EOExpr[Fix[EOExpr]],
    parameterMap: Map[String, EOExpr[Fix[EOExpr]]]
  ): EOExpr[Fix[EOExpr]] = {

    def exprHelper(expr: EOExpr[Fix[EOExpr]]): Fix[EOExpr] = expr match {
      case EOObj(freeAttrs, varargAttr, bndAttrs) =>
        Fix(EOObj(freeAttrs, varargAttr, bndAttrs.map(eoBndHelper)))
      case EOCopy(Fix(trg), args) =>
        Fix(EOCopy(exprHelper(trg), args.map(bndHelper)))
      case EODot(Fix(src), name) => Fix(EODot(exprHelper(src), name))
      case EOSimpleApp(name) if parameterMap.contains(name) =>
        Fix(parameterMap(name))
      case id => Fix(id)
    }

    def eoBndHelper(bnd: EOBndExpr[Fix[EOExpr]]): EOBndExpr[Fix[EOExpr]] =
      EOBndExpr(bnd.bndName, exprHelper(Fix.un(bnd.expr)))

    def bndHelper(bnd: EOBnd[Fix[EOExpr]]): EOBnd[Fix[EOExpr]] = bnd match {
      case EOAnonExpr(Fix(expr)) => EOAnonExpr(exprHelper(expr))
      case EOBndExpr(tmp, Fix(expr)) => EOBndExpr(tmp, exprHelper(expr))
    }

    Fix.un(exprHelper(methodBody))
  }

  def inlineCalls(
    code: EOProg[Fix[EOExpr]],
    targets: List[EOCopy[Fix[EOExpr]]],
    replacementMethodBody: EOExpr[Fix[EOExpr]],
    replacementMethodArgs: Vector[String],
    replacementMethodLocalBnds: Vector[EOBndExpr[Fix[EOExpr]]]
  ): EOProg[Fix[EOExpr]] = {
    def exprHelper(expr: EOExpr[Fix[EOExpr]]): Fix[EOExpr] = expr match {
      case EOObj(freeAttrs, varargAttr, bndAttrs) =>
        Fix(EOObj(freeAttrs, varargAttr, bndAttrs.map(eoBndHelper)))
      case copy @ EOCopy(trg, args) =>
        lazy val recurse = EOCopy(trg, args.map(bndHelper))

        if (targets.contains(copy)) {
          val parameterMap: Map[String, EOExpr[Fix[EOExpr]]] = Map
            .from(
              replacementMethodArgs.zip(
                copy.args.value.map(bnd => Fix.un(bnd.expr))
              )
            )
          val bndMap: Map[String, EOExpr[Fix[EOExpr]]] = Map
            .from(
              replacementMethodLocalBnds.map(bnd =>
                (bnd.bndName.name.name, Fix.un(bnd.expr))
              )
            )
          val replacedBndsBody = propagateParams(replacementMethodBody, bndMap)
          Fix(propagateParams(replacedBndsBody, parameterMap))
        } else {
          Fix(recurse)
        }

      case id => Fix(id)
    }

    def eoBndHelper(bnd: EOBndExpr[Fix[EOExpr]]): EOBndExpr[Fix[EOExpr]] =
      EOBndExpr(bnd.bndName, exprHelper(Fix.un(bnd.expr)))

    def bndHelper(bnd: EOBnd[Fix[EOExpr]]): EOBnd[Fix[EOExpr]] = bnd match {
      case EOAnonExpr(Fix(expr)) => EOAnonExpr(exprHelper(expr))
      case EOBndExpr(tmp, Fix(expr)) => EOBndExpr(tmp, exprHelper(expr))
    }

    EOProg(code.metas, code.bnds.map(bndHelper))
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val sourcecode = readFile("mana.eo")
    for {
      code <- EoParser.sourceCodeEoParser[IO](2).parse(sourcecode)
      _ <- IO.delay(astPrinter.pprintln(code))

      // Task 1
      _ <- IO.println("-" * 100 ++ "Task1")
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
      methodBody <- IO.fromOption(
        checkManaMethod
          .bndAttrs
          .find {
            case EOBndExpr(EODecoration, _) => true
            case _ => false
          }
          .map(bnd => Fix.un(bnd.expr))
      )(new Exception("Method has no Body!!!"))
      methodArgs = checkManaMethod.freeAttrs.map(_.name)
      methodBnds = checkManaMethod
        .bndAttrs
        .filter {
          case _ => true
          case EOBndExpr(EODecoration, _) => false
        }

      inlinedCheckManaMethod =
        inlineCalls(code, targets, methodBody, methodArgs, methodBnds)
      _ <- IO.println(inlinedCheckManaMethod.toEO.allLinesToString)
    } yield ExitCode.Success
  }

}
