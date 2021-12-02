package com.github.fng.adventofcode2021.day02

object Day02Part2 {

  object CourseParser {

    def parse(lines: List[String]): List[Movement] = {
      lines.map { line =>
        line.split(" ").toList match {
          case List(direction, unit) =>
            direction match {
              case "forward" => Forward(unit.toInt)
              case "down"    => Down(unit.toInt)
              case "up"      => Up(unit.toInt)
              case other     => sys.error(s"Unexpected direction: $other")
            }
          case other => sys.error(s"Unexpected movement to parse: ${other}")
        }
      }
    }

  }

  object CruiseControl {
    def followCourse(
        startPosition: Position,
        course: List[Movement]
    ): Position = {
      course.foldLeft(startPosition) { case (currentPosition, movement) =>
        movement.move(currentPosition)
      }
    }
  }

  case class Position(horizontalPosition: Int, depth: Int, aim: Int) {
    def product: Int = horizontalPosition * depth
  }

  sealed trait Movement {
    def move(currentPosition: Position): Position
  }

  class HorizontalMovement(units: Int) extends Movement {
    override def move(currentPosition: Position): Position =
      currentPosition.copy(
        horizontalPosition = currentPosition.horizontalPosition + units,
        depth = currentPosition.depth + (units * currentPosition.aim)
      )
  }

  class AdjustAim(units: Int) extends Movement {
    override def move(currentPosition: Position): Position =
      currentPosition.copy(aim = currentPosition.aim + units)
  }

  case class Forward(units: Int) extends HorizontalMovement(units) {
    require(units > 0, "units for movement must be positive")
  }

  case class Down(units: Int) extends AdjustAim(units) {
    require(units > 0, "units for movement must be positive")
  }

  case class Up(units: Int) extends AdjustAim(units * -1) {
    require(units > 0, "units for movement must be positive")
  }
}
