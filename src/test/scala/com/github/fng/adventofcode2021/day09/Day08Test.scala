package com.github.fng.adventofcode2021.day09

import com.github.fng.adventofcode2021.ResourceUtils
import org.scalatest.funsuite.AnyFunSuite

class Day09Test extends AnyFunSuite {

  test("Day09 - Part1 - reference") {
    val input = ResourceUtils.getLinesFromResource("day09/reference-input.txt")
    val map = Day09.parseInputToMap(input)

    val mapAsString = 0
      .to(4)
      .map { y =>
        0.to(9)
          .map { x =>
            map((x, y))
          }
          .mkString
      }
      .mkString("\n")

    println(mapAsString === """2199943210
                              |3987894921
                              |9856789892
                              |8767896789
                              |9899965678""".stripMargin)

    println(
      Day09.lowestPoints(map) === List(
        ((2, 2), 5),
        ((6, 4), 5),
        ((9, 0), 0),
        ((1, 0), 1)
      )
    )
    println(Day09.calculateRiskLevel(map) === 15)

  }

  test("Day09 - Part1 - exercise") {

    val input = ResourceUtils.getLinesFromResource("day09/input.txt")
    val map = Day09.parseInputToMap(input)

    assert(Day09.calculateRiskLevel(map) === 423)
  }

  test("Day09 - Part2 - reference") {

    val input = ResourceUtils.getLinesFromResource("day09/reference-input.txt")
    val map = Day09.parseInputToMap(input)
    val lowPoints = Day09.lowestPoints(map)

    val firstLowPoint = lowPoints.head._1
    val firstLowPointBasin = Day09.basinForLowPoint(map, firstLowPoint)

    val firstLowPointBasinAsString = 0
      .to(4)
      .map { y =>
        0.to(9)
          .map { x =>
            firstLowPointBasin.getOrElse((x, y), "-")
          }
          .mkString
      }
      .mkString("\n")

    assert(firstLowPointBasinAsString === """----------
                                            |--878-----
                                            |-85678----
                                            |87678-----
                                            |-8--------""".stripMargin)

    assert(
      Day09.calculateLargestBasinsProduct(map, lowPoints.map(_._1)) === 1134
    )
  }

  test("Day09 - Part2 - exercise") {
    val input = ResourceUtils.getLinesFromResource("day09/input.txt")
    val map = Day09.parseInputToMap(input)
    val lowPoints = Day09.lowestPoints(map)
    assert(
      Day09.calculateLargestBasinsProduct(map, lowPoints.map(_._1)) === 1198704
    )
  }

}
