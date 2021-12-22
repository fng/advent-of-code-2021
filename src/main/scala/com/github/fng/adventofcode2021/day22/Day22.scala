package com.github.fng.adventofcode2021.day22

object Day22 {

  case class Point(x: Int, y: Int, z: Int)

  case class Instruction(
      on: Boolean,
      xRange: Range.Inclusive,
      yRange: Range.Inclusive,
      zRang: Range.Inclusive
  ) {
    lazy val points: List[Point] = {
      xRange.flatMap { x =>
        yRange.flatMap { y =>
          zRang.map { z =>
            Point(x, y, z)
          }
        }
      }.toList
    }

    lazy val numberOfPoints: Long = xRange.length * yRange.length * zRang.length
  }

  case class Instructions(instructions: List[Instruction]) {
    def findTurnedOnCubes(): Set[Point] = {
      instructions.foldLeft(Set[Point]()) {
        case (turnedOnPoints, instruction) =>
          println(s"$instruction with ${instruction.numberOfPoints}")
          if (instruction.on) {
            turnedOnPoints ++ instruction.points.toSet
          } else {
            turnedOnPoints -- instruction.points.toSet
          }
      }
    }
  }

  def parse(lines: List[String]): Instructions = {
    Instructions(lines.map { line =>
      line.split(" ").toList match {
        case List(on, rest) =>
          rest.split(",").toList match {
            case List(x, y, z) =>
              val onOrOff = on match {
                case "on"  => true
                case "off" => false
                case _     => sys.error(s"can't parse: $on")
              }
              Instruction(onOrOff, parseRange(x), parseRange(y), parseRange(z))
            case _ => sys.error(s"can't parse: $rest")
          }
        case _ => sys.error(s"can't parse: $line")
      }
    })
  }

  private def parseRange(range: String): Range.Inclusive = {
    range.drop(2).split("\\.\\.").toList match {
      case List(start, end) => Range.inclusive(start.toInt, end.toInt)
      case _                => sys.error(s"can't parse: $range")
    }
  }

}
