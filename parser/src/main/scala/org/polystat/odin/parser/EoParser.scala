package org.polystat.odin.parser

import cats.{ApplicativeError, Functor, MonadError}
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.either._
import org.polystat.odin.core.ast.{EOBnd, EOMetas, EOProg}
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.xmir.XmirToAst
import org.polystat.odin.parser.xmir.XmirToAst.parseXMIR

trait EoParser[EORepr, F[_], R] {
  def parse(eoRepr: EORepr): F[R]
}

object EoParser {

  def apply[EORepr, F[_], R](implicit
    parser: EoParser[EORepr, F, R]
  ): EoParser[EORepr, F, R] = parser

  def parse[EORepr, F[_], R](eoRepr: EORepr)(implicit
    parser: EoParser[EORepr, F, R]
  ): F[R] = parser.parse(eoRepr)

  implicit def sourceCodeEoParser[F[_]](
    indentationStep: Int = 2
  )(implicit
    ae: ApplicativeError[F, Throwable]
  ): EoParser[String, F, EOProg[EOExprOnly]] =
    new EoParser[String, F, EOProg[EOExprOnly]] {
      import fastparse.Parser

      override def parse(
        eoRepr: String
      ): F[EOProg[EOExprOnly]] = Parser
        .parse(eoRepr, indentationStep)
        .fold(
          onFailure = (label, index, extra) => {
            val errorMessage =
              new IllegalArgumentException(
                s"""[$index] Parsing failed with error: $label. Extra:
                   |$extra""".stripMargin
              )
            ApplicativeError[F, Throwable].raiseError(errorMessage)
          },
          onSuccess = (ast, _) => ApplicativeError[F, Throwable].pure(ast)
        )

    }

  implicit def xmirToEoBndEoParser[EORepr, F[_]](implicit
    xmirParser: XmirToAst[F, EORepr],
    me: MonadError[F, Throwable]
  ): EoParser[EORepr, F, Vector[EOBnd[EOExprOnly]]] =
    new EoParser[EORepr, F, Vector[EOBnd[EOExprOnly]]] {

      override def parse(eoRepr: EORepr): F[Vector[EOBnd[EOExprOnly]]] = for {
        parseResult <- parseXMIR[F, EORepr](eoRepr)
        result <- MonadError[F, Throwable].fromEither(
          parseResult.leftMap(new RuntimeException(_))
        )
      } yield result

    }

  implicit def xmirToEoProgEoParser[EORepr, F[_]: Functor](implicit
    parser: EoParser[EORepr, F, Vector[EOBnd[EOExprOnly]]]
  ): EoParser[EORepr, F, EOProg[EOExprOnly]] =
    new EoParser[EORepr, F, EOProg[EOExprOnly]] {

      override def parse(
        eoRepr: EORepr
      ): F[EOProg[EOExprOnly]] =
        parser.parse(eoRepr).map(EOProg(EOMetas(None, Vector()), _))

    }

}
