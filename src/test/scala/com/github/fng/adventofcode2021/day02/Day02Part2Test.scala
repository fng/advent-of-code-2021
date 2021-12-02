package com.github.fng.adventofcode2021.day02

import com.github.fng.adventofcode2021.ResourceUtils
import com.github.fng.adventofcode2021.day02.Day02Part2._
import org.scalatest.funsuite.AnyFunSuite

class Day02Part2Test extends AnyFunSuite {

  test("Forward increased the horizontal position only") {
    val start = Position(horizontalPosition = 10, depth = 50, aim = 0)
    assert(
      Forward(5)
        .move(start) === Position(horizontalPosition = 15, depth = 50, aim = 0)
    )
  }

  test("Down increased the aim only") {
    val start = Position(horizontalPosition = 10, depth = 50, aim = 20)
    assert(
      Down(5)
        .move(start) === Position(horizontalPosition = 10, depth = 50, aim = 25)
    )
  }

  test("Up increased the depth only") {
    val start = Position(horizontalPosition = 10, depth = 50, aim = 20)
    assert(
      Up(5)
        .move(start) === Position(horizontalPosition = 10, depth = 50, aim = 15)
    )
  }

  test("CruiseControl follows the course") {
    val start = Position(horizontalPosition = 10, depth = 50, aim = 0)
    val end = CruiseControl.followCourse(
      start,
      List(
        Forward(5),
        Down(5),
        Forward(2),
        Up(3),
        Forward(3)
      )
    )
    assert(end === Position(horizontalPosition = 20, depth = 66, aim = 2))
  }

  test("CourseParser turns list of string instructions into a course") {
    val input = List("forward 5", "down 10", "up 3")
    val course = List(
      Forward(5),
      Down(10),
      Up(3)
    )

    assert(CourseParser.parse(input) === course)
  }

  test("Day02 - Part 1 - reference") {
    val startPosition = Position(0, 0, 0)
    val course = CourseParser.parse(
      ResourceUtils.getLinesFromResource("day02/reference-input.txt")
    )
    val endPosition = CruiseControl.followCourse(startPosition, course)
    assert(endPosition === Position(15, 60, 10))
    assert(endPosition.product === 900)
  }

  test("Day02 - Part 1 - exercise") {
    val startPosition = Position(0, 0, 0)
    val course =
      CourseParser.parse(ResourceUtils.getLinesFromResource("day02/input.txt"))
    val endPosition = CruiseControl.followCourse(startPosition, course)
    assert(endPosition === Position(1980, 995572, 951))
    assert(endPosition.product === 1971232560)
  }

}
