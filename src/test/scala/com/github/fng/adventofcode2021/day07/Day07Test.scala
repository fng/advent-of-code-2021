package com.github.fng.adventofcode2021.day07

import com.github.fng.adventofcode2021.ResourceUtils
import org.scalatest.funsuite.AnyFunSuite

class Day07Test extends AnyFunSuite {

  test("Day07 - Part1 - reference") {

    val input = ResourceUtils.getLinesFromResource("day07/reference-input.txt")
    val startPositions = Day07.parseInput(input)

    assert(startPositions === List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14))

    assert(Day07.Part1.fuelCostToMoveAllTo(startPositions, 2) === 37)
    assert(Day07.Part1.fuelCostToMoveAllTo(startPositions, 1) === 41)
    assert(Day07.Part1.fuelCostToMoveAllTo(startPositions, 3) === 39)
    assert(Day07.Part1.fuelCostToMoveAllTo(startPositions, 10) === 71)

    assert(Day07.Part1.alignToPositionWithLeastFuel(startPositions) === (2, 37))

  }

  test("Day07 - Part1 - exercise") {
    val input = ResourceUtils.getLinesFromResource("day07/input.txt")
    val startPositions = Day07.parseInput(input)
    assert(
      Day07.Part1.alignToPositionWithLeastFuel(startPositions) === (336, 344735)
    )
  }

  test("Day07 - Part2 - reference") {
    val input = ResourceUtils.getLinesFromResource("day07/reference-input.txt")
    val startPositions = Day07.parseInput(input)

    assert(Day07.Part2.fuelCostToMoveAllTo(startPositions, 2) === 206)
    assert(Day07.Part2.fuelCostToMoveAllTo(startPositions, 5) === 168)

    assert(
      Day07.Part2.alignToPositionWithLeastFuel(startPositions) === (5, 168)
    )

  }

  test("Day07 - Part2 - exercise") {
    val input = ResourceUtils.getLinesFromResource("day07/input.txt")
    val startPositions = Day07.parseInput(input)
    assert(
      Day07.Part2.alignToPositionWithLeastFuel(
        startPositions
      ) === (474, 96798233)
    )
  }

}
