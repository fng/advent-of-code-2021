package com.github.fng.adventofcode2021.day13

object Day13 {

  case class Point(x: Int, y: Int)

  sealed trait Fold
  case class VerticalFoldLine(line: Int) extends Fold
  case class HorizontalFoldLine(line: Int) extends Fold

  case class Instruction(
      holes: List[Point],
      folds: List[Fold],
      paperSize: (Int, Int)
  ) {

    def toPaperWithHoles: String = {
      val maxX = paperSize._1
      val maxY = paperSize._2

      0.to(maxY)
        .map { y =>
          0.to(maxX)
            .map { x =>
              if (holes.contains(Point(x, y))) "#" else "."
            }
            .mkString
        }
        .mkString("\n")
    }

    def foldOnce: Instruction = {
      if (folds.isEmpty) {
        this
      } else {
        val foldInstruction = folds.head

        foldInstruction match {
          case VerticalFoldLine(foldAtX) =>
            val (leftFoldLine, rightFoldLine) = holes.partition(_.x < foldAtX)

            val rightPointsFoldedLeft = rightFoldLine.map { point =>
              point.copy(x = point.x - (point.x - foldAtX) * 2)
            }

            val newHoles = (leftFoldLine ++ rightPointsFoldedLeft).distinct
            Instruction(
              newHoles,
              folds.tail,
              this.paperSize.copy(_1 = foldAtX - 1)
            )

          case HorizontalFoldLine(foldAtY) =>
            val (aboveFoldLine, belowFoldLine) = holes.partition(_.y < foldAtY)

            val belowPointsFoldedUp = belowFoldLine.map { point =>
              point.copy(y = point.y - (point.y - foldAtY) * 2)
            }

            val newHoles = (aboveFoldLine ++ belowPointsFoldedUp).distinct
            Instruction(
              newHoles,
              folds.tail,
              this.paperSize.copy(_2 = foldAtY - 1)
            )
        }
      }
    }

    def foldAll: Instruction = {
      1.to(folds.length).foldLeft(this) { case (instruction, fold) =>
        instruction.foldOnce
      }
    }

  }

  def parseInstructions(lines: List[String]): Instruction = {
    val emptyLineIndex = lines.indexWhere(_.isEmpty)
    val holes = lines.take(emptyLineIndex).map { point =>
      point.split(",").toList match {
        case List(x, y) => Point(x.toInt, y.toInt)
        case _          => sys.error(s"Can't parse point: $point")
      }
    }

    val foldLines = lines.drop(emptyLineIndex + 1).map { foldLine =>
      foldLine.stripPrefix("fold along ").split("=").toList match {
        case List("x", line) => VerticalFoldLine(line.toInt)
        case List("y", line) => HorizontalFoldLine(line.toInt)
        case other           => sys.error(s"Can't parse fold line: $foldLine")
      }
    }

    val maxX = holes.map(_.x).max
    val maxY = holes.map(_.y).max

    Instruction(holes, foldLines, (maxX, maxY))
  }

}
