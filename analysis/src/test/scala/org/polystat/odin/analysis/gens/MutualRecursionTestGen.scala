package org.polystat.odin.analysis.gens

import CallGraph._
import cats.effect.IO
import cats.effect.kernel.Sync
import cats.effect.unsafe.implicits.global
import fs2.io.file.{Files, Path}
import org.scalacheck.Gen
import fs2.Stream
import fs2.text.utf8
import cats.syntax.flatMap._

import scala.util.Try

object MutualRecursionTestGen {

  // TODO: allow this to generate programs with more than 26 objects
  def genObjectName(
    p: Program,
    containerObjName: Option[ObjectName]
  ): Gen[ObjectName] = {
    Gen
      .listOfN(1, Gen.alphaLowerChar)
      .map(_.mkString)
      .retryUntil(!p.containsObjectWithName(_))
      .map(ObjectName(containerObjName, _))
  }

  def between[T](min: Int, max: Int, g: Gen[T]): Gen[List[T]] =
    for {
      n <- Gen.choose(min, max)
      lst <- Gen.listOfN(n, g)
    } yield lst

  def pickOneOrZero[T](lst: List[T]): Gen[Set[T]] =
    for {
      n <- Gen.oneOf(0, 1)
      lst <- Try(Gen.pick(n, lst)).fold(
        _ => Gen.const(Set.empty[T]),
        gen => gen.map(_.toSet)
      )
    } yield lst

  def randomlySplit[T](list: List[T]): Gen[(List[T], List[T])] =
    for {
      part1 <- Gen
        .atLeastOne(list)
        .map(_.toList)
      part2 = list.filter(!part1.contains(_))
    } yield (part1, part2)

  def genMethodName: Gen[String] =
    Gen
      .listOfN(1, Gen.alphaLowerChar)
      .map(_.mkString)

  def genCallGraph(
    methodNamesToDefine: List[MethodName],
    methodNamesToCall: List[MethodName]
  ): Gen[CallGraph] = {
    for {
      calls <- Gen.sequence[CallGraph, CallGraphEntry](
        methodNamesToDefine.map(method =>
          pickOneOrZero(methodNamesToCall.filter(_ != method))
            .map(calls => (method, calls))
        )
      )

    } yield calls
  }

  // TODO: add containerObj parameter
  def genExtendObject(obj: Object, p: Program): Gen[Object] = for {
    // name for new object
    objName <- genObjectName(p, None)

    // new method definitions
    newMethodNames <-
      between(1, 2, genMethodName)
        .retryUntil(names =>
          names.forall(n => !obj.callGraph.containsMethodWithName(n))
        )
        .map(_.map(MethodName(objName, _)))

    // redefined methods, e.g. methods with the same name as those from the
    // parent object
    (redefinedMethodNames, otherMethodNames) <-
      randomlySplit(
        obj.callGraph.keys.toList
      )

    methodNamesToDefine = newMethodNames ++ redefinedMethodNames.map(method =>
      // replacing the object part of the method name with the new object
      // so, method 'a.s' becomes 'name.s'
      MethodName(objName, method.name)
    )

    // new methods may call any other method, both new and old
    methodNamesToCall = methodNamesToDefine ++ otherMethodNames
    callGraph <- genCallGraph(methodNamesToDefine, methodNamesToCall)

  } yield obj.extended(objName, callGraph)

  def genObject(p: Program, containerObj: Option[ObjectName]): Gen[Object] =
    for {
      objectName <- genObjectName(p, containerObj)
      nestedObjects <- Gen.frequency(
        4 -> Gen.const(List()),
        1 -> (for {
          n <- Gen.choose(1, 3)
          a <- (0 until n).foldLeft[Gen[List[Object]]](Gen.const(List()))(
            (accGen, _) =>
              for {
                acc <- accGen
                obj <- genObject(Program(acc), Some(objectName))
              } yield acc ++ List(obj)
          )
        } yield a)
      )
      methods <-
        between(1, 4, genMethodName)
          .map(_.map(MethodName(objectName, _)))
      cg <- genCallGraph(methods, methods)
    } yield Object(
      name = objectName,
      ext = None,
      nestedObjs = nestedObjects,
      callGraph = cg,
    )

  def genProgram(size: Int): Gen[Program] = {
    def get_inner_objs(obj: Object): List[Object] = {
      obj.nestedObjs.flatMap(get_inner_objs)
    }

    for {
      initObj <-
        genObject(Program(Nil), None).retryUntil(obj =>
          !obj.callGraph.containsCycles
        )
      init = Gen.const(Program(List(initObj)))
      program <- (1 until size).foldLeft(init) { case (acc, _) =>
        for {
          extend <- Gen.frequency(
            (1, true),
            (1, false)
          )
          prog <- acc.flatMap(p =>
            (
              if (extend) {
                // TODO: account for the nested objects
                Gen
                  .oneOf(p.objs ++ p.objs.flatMap(get_inner_objs))
                  .flatMap(obj =>
                    genExtendObject(obj, Program(get_inner_objs(obj)))
                  )
                  .retryUntil(obj => !obj.callGraph.containsSingleObjectCycles)
              } else
                genObject(p, None).retryUntil(obj =>
                  !obj.callGraph.containsCycles
                )
            ).map(newObj => Program(p.objs ++ List(newObj)))
          )
        } yield prog
      }
    } yield program
  }

  def generateProgramFiles[F[_]: Files: Sync](
    n: Int,
    dir: Path,
    programGen: Gen[Program],
    converters: List[(Program => String, String)]
  ): Stream[F, Unit] =
    for {
      i <- Stream.range(1, n + 1)
      prog <- Stream.eval(retryUntilComplete(programGen))
      (convert, ext) <- Stream.emits(converters)
      _ <- Stream
        .emit(prog)
        .map(convert)
        .through(utf8.encode)
        .through(
          Files[F].writeAll(dir.resolve(s"$i.$ext"))
        )
        .as(())
    } yield ()

  def retryUntilComplete[F[_]: Sync, T](g: Gen[T]): F[T] = {
    Sync[F]
      .attempt(
        Sync[F].delay(g.sample.get)
      )
      .flatMap {
        case Left(_) => retryUntilComplete(g)
        case Right(value) => Sync[F].pure(value)
      }
  }

  def textFromProgram(
    prog: Program,
    commentMarker: String,
    display: Object => String
  ): String = {
    val cycles = prog
      .objs
      .flatMap(_.callGraph.findCycles.map(cc => commentMarker + cc.show))
      .mkString("\n")

    val progText = prog.objs.map(display).mkString("\n")

    s"""
       |$cycles
       |
       |$progText
       |""".stripMargin

  }

  def main(args: Array[String]): Unit = {
    generateProgramFiles[IO](
      n = 20,
      dir = Path("analysis/src/test/resources/mutualrec/generated"),
      programGen = genProgram(3).retryUntil(p =>
        p.objs.exists(_.callGraph.containsMultiObjectCycles)
      ),
      converters = List(
        (p => textFromProgram(p, "# ", _.toEO), "eo"),
        (p => textFromProgram(p, "// ", _.toCPP), "cpp"),
      )
    )
      .compile
      .drain
      .unsafeRunSync()
  }

}
