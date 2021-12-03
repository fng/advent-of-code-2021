package com.github.fng.adventofcode2021.day03

import com.github.fng.adventofcode2021.ResourceUtils
import org.scalatest.funsuite.AnyFunSuite

class Day03Test extends AnyFunSuite {

  test("Day03 - Part 1 - reference") {
    val diagnosticReport =
      ResourceUtils.getLinesFromResource("day03/reference-input.txt")
    val mostCommonBitByIndex = Day03.mostCommonBitByIndex(diagnosticReport)
    val leastCommonBitByIndex =
      Day03.inverseCommonBitByIndex(mostCommonBitByIndex)
    val mostCommonAsDecimal = Day03.binaryToDecimal(mostCommonBitByIndex)
    val leastCommonAsDecimal = Day03.binaryToDecimal(leastCommonBitByIndex)
    assert(mostCommonBitByIndex === "10110")
    assert(leastCommonBitByIndex === "01001")
    assert(mostCommonAsDecimal === 22)
    assert(leastCommonAsDecimal === 9)

    assert(Day03.calculatePowerConsumption(diagnosticReport) === 198)
  }

  test("Day03 - Part 1 - exercise") {
    val diagnosticReport = ResourceUtils.getLinesFromResource("day03/input.txt")

    val mostCommonBitByIndex = Day03.mostCommonBitByIndex(diagnosticReport)
    val leastCommonBitByIndex =
      Day03.inverseCommonBitByIndex(mostCommonBitByIndex)

    assert(mostCommonBitByIndex === "100011100101")
    assert(leastCommonBitByIndex === "011100011010")

    assert(Day03.binaryToDecimal(mostCommonBitByIndex) === 2277)
    assert(Day03.binaryToDecimal(leastCommonBitByIndex) === 1818)

    assert(Day03.calculatePowerConsumption(diagnosticReport) === 4139586)
  }
//
//  test("Day01 - Part 2 - reference") {
//    val numbers =
//      ResourceUtils
//        .getLinesFromResource("day01/reference-input.txt")
//        .map(_.toInt)
//    assert(
//      Day01.Part2.numberOfTimesANumberIncreasesWithSlidingWindow(numbers) === 5
//    )
//  }
//
//  test("Day01 - Part 2 - exercise") {
//    val numbers =
//      ResourceUtils.getLinesFromResource("day01/input.txt").map(_.toInt)
//    assert(
//      Day01.Part2.numberOfTimesANumberIncreasesWithSlidingWindow(
//        numbers
//      ) === 1822
//    )
//  }
}
