package com.github.fng.adventofcode2021.day01

import com.github.fng.adventofcode2021.ResourceUtils
import org.scalatest.funsuite.AnyFunSuite

class Day01Test extends AnyFunSuite {

  test("Day01 - reference") {
    val numbers =
      ResourceUtils.getLinesFromResource("day01/reference-input.txt")
    assert(Day01.numberOfTimesANumberIncreases(numbers) === 7)
  }

  test("Day01 - exercise") {
    val numbers = ResourceUtils.getLinesFromResource("day01/input.txt")
    assert(Day01.numberOfTimesANumberIncreases(numbers) === 1789)
  }

}
