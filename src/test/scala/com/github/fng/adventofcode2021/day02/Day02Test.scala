package com.github.fng.adventofcode2021.day02

import com.github.fng.adventofcode2021.ResourceUtils
import com.github.fng.adventofcode2021.day01.Day01
import com.github.fng.adventofcode2021.day02.Day02._
import org.scalatest.funsuite.AnyFunSuite

class Day02Test extends AnyFunSuite {

  test("Forward increased the horizontal position only") {
    val start = Position(horizontalPosition = 10, depth = 50)
    assert(
      Forward(5).move(start) === Position(horizontalPosition = 15, depth = 50)
    )
  }

  test("Down increased the depth only") {
    val start = Position(horizontalPosition = 10, depth = 50)
    assert(
      Down(5).move(start) === Position(horizontalPosition = 10, depth = 55)
    )
  }

  test("Up increased the depth only") {
    val start = Position(horizontalPosition = 10, depth = 50)
    assert(Up(5).move(start) === Position(horizontalPosition = 10, depth = 45))
  }

  test("CruiseControl follows the course") {
    val start = Position(horizontalPosition = 10, depth = 50)
    val end = CruiseControl.followCourse(
      start,
      List(
        Forward(5),
        Down(5),
        Up(3)
      )
    )
    assert(end === Position(horizontalPosition = 15, depth = 52))
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
    val startPosition = Position(0, 0)
    val course = CourseParser.parse(
      ResourceUtils.getLinesFromResource("day02/reference-input.txt")
    )
    val endPosition = CruiseControl.followCourse(startPosition, course)
    assert(endPosition === Position(15, 10))
    assert(endPosition.product === 150)
  }

  test("Day02 - Part 1 - exercise") {
    val startPosition = Position(0, 0)
    val course =
      CourseParser.parse(ResourceUtils.getLinesFromResource("day02/input.txt"))
    val endPosition = CruiseControl.followCourse(startPosition, course)
    assert(endPosition === Position(1980, 951))
    assert(endPosition.product === 1882980)
  }

}
