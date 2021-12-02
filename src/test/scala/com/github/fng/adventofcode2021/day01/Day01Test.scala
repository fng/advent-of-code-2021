package com.github.fng.adventofcode2021.day01

import com.github.fng.adventofcode2021.ResourceUtils
import org.scalatest.funsuite.AnyFunSuite

class Day01Test extends AnyFunSuite {

  test("Day01 - Part 1 - reference") {
    val numbers =
      ResourceUtils
        .getLinesFromResource("day01/reference-input.txt")
        .map(_.toInt)
    assert(Day01.Part1.numberOfTimesANumberIncreases(numbers) === 7)
  }

  test("Day01 - Part 1 - exercise") {
    val numbers =
      ResourceUtils.getLinesFromResource("day01/input.txt").map(_.toInt)
    assert(Day01.Part1.numberOfTimesANumberIncreases(numbers) === 1791)
  }

  test("Day01 - Part 2 - reference") {
    val numbers =
      ResourceUtils
        .getLinesFromResource("day01/reference-input.txt")
        .map(_.toInt)
    assert(
      Day01.Part2.numberOfTimesANumberIncreasesWithSlidingWindow(numbers) === 5
    )
  }

  test("Day01 - Part 2 - exercise") {
    val numbers =
      ResourceUtils.getLinesFromResource("day01/input.txt").map(_.toInt)
    assert(
      Day01.Part2.numberOfTimesANumberIncreasesWithSlidingWindow(
        numbers
      ) === 1822
    )
  }
}
