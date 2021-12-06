package com.github.fng.adventofcode2021.day05

import com.github.fng.adventofcode2021.ResourceUtils
import com.github.fng.adventofcode2021.day04.Day04.{Board, DrawnNumbers}
import com.github.fng.adventofcode2021.day05.Day05.Point
import org.scalatest.funsuite.AnyFunSuite

class Day05Test extends AnyFunSuite {

  test("Day05 - Part1 - reference") {

    val input = ResourceUtils.getLinesFromResource("day05/reference-input.txt")

    val fromToPoints = Day05.parseInput(input)

    assert(
      fromToPoints === List(
        (Point(0, 9), Point(5, 9)),
        (Point(8, 0), Point(0, 8)),
        (Point(9, 4), Point(3, 4)),
        (Point(2, 2), Point(2, 1)),
        (Point(7, 0), Point(7, 4)),
        (Point(6, 4), Point(2, 0)),
        (Point(0, 9), Point(2, 9)),
        (Point(3, 4), Point(1, 4)),
        (Point(0, 0), Point(8, 8)),
        (Point(5, 5), Point(8, 2))
      )
    )

    val diagram = Day05.Part1.generateDiagram(fromToPoints)
    println(diagram)

    assert(diagram.score === 5)

  }

  test("Day05 - Part1 - exercise") {

    val input = ResourceUtils.getLinesFromResource("day05/input.txt")

    val fromToPoints = Day05.parseInput(input)
    val diagram = Day05.Part1.generateDiagram(fromToPoints)

    assert(diagram.score === 5835)

  }

  test("Day05 - Part2 - reference") {

    val input = ResourceUtils.getLinesFromResource("day05/reference-input.txt")

    val fromToPoints = Day05.parseInput(input)

    val diagram = Day05.Part2.generateDiagram(fromToPoints)
    println(diagram)

    assert(diagram.score === 12)

  }

  test("Day05 - Part2 - exercise") {

    val input = ResourceUtils.getLinesFromResource("day05/input.txt")

    val fromToPoints = Day05.parseInput(input)

    val diagram = Day05.Part2.generateDiagram(fromToPoints)

    assert(diagram.score === 17013)
  }

}
