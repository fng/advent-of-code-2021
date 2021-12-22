package com.github.fng.adventofcode2021.day22

import com.github.fng.adventofcode2021.ResourceUtils
import com.github.fng.adventofcode2021.day22.Day22.Point
import org.scalatest.funsuite.AnyFunSuite

class Day22Test extends AnyFunSuite {

  test("Day22 - Part 1 - example") {
    val instructions = Day22.parse(
      List(
        "on x=10..12,y=10..12,z=10..12",
        "on x=11..13,y=11..13,z=11..13",
        "off x=9..11,y=9..11,z=9..11",
        "on x=10..10,y=10..10,z=10..10"
      )
    )
    assert(
      instructions.instructions.head.points === List(
        Point(10, 10, 10),
        Point(10, 10, 11),
        Point(10, 10, 12),
        Point(10, 11, 10),
        Point(10, 11, 11),
        Point(10, 11, 12),
        Point(10, 12, 10),
        Point(10, 12, 11),
        Point(10, 12, 12),
        Point(11, 10, 10),
        Point(11, 10, 11),
        Point(11, 10, 12),
        Point(11, 11, 10),
        Point(11, 11, 11),
        Point(11, 11, 12),
        Point(11, 12, 10),
        Point(11, 12, 11),
        Point(11, 12, 12),
        Point(12, 10, 10),
        Point(12, 10, 11),
        Point(12, 10, 12),
        Point(12, 11, 10),
        Point(12, 11, 11),
        Point(12, 11, 12),
        Point(12, 12, 10),
        Point(12, 12, 11),
        Point(12, 12, 12)
      )
    )
    assert(instructions.findTurnedOnCubes().size === 39)
  }

  test("Day22 - Part 1 - reference") {
    val input =
      ResourceUtils.getLinesFromResource("day22/reference-input-part1.txt")
    val instructions = Day22.parse(input)
    assert(instructions.findTurnedOnCubes().size === 590784)
  }

  test("Day22 - Part 1 - exercise") {
    val input = ResourceUtils.getLinesFromResource("day22/input-part1.txt")
    val instructions = Day22.parse(input)
    assert(instructions.findTurnedOnCubes().size === 580098)
  }

//  test("Day22 - Part 2 - exercise") {
//    val input = ResourceUtils.getLinesFromResource("day22/input-part2.txt")
//    val instructions = Day22.parse(input)
//    assert(instructions.findTurnedOnCubes().size === 580098)
//  }

}
