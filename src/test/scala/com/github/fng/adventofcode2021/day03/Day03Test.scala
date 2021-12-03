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

  test("Day03 - Part 2 - reference") {
    val diagnosticReport =
      ResourceUtils.getLinesFromResource("day03/reference-input.txt")

    val filteredByMostCommonBit =
      Day03.filterByCommonBit(diagnosticReport, filterMostCommon = true)
    val filteredByLeastCommonBit =
      Day03.filterByCommonBit(diagnosticReport, filterMostCommon = false)

    assert(filteredByMostCommonBit === List("10111"))
    assert(filteredByLeastCommonBit === List("01010"))

    assert(Day03.binaryToDecimal(filteredByMostCommonBit.head) === 23)
    assert(Day03.binaryToDecimal(filteredByLeastCommonBit.head) === 10)

    assert(Day03.determineOxygenGeneratorRating(diagnosticReport) === 23)
    assert(Day03.determineC02ScrubberRating(diagnosticReport) === 10)
    assert(Day03.determineLifesupportRating(diagnosticReport) === 230)
  }

  test("Day03 - Part 2 - exercise") {
    val diagnosticReport =
      ResourceUtils.getLinesFromResource("day03/input.txt")

    val filteredByMostCommonBit =
      Day03.filterByCommonBit(diagnosticReport, filterMostCommon = true)
    val filteredByLeastCommonBit =
      Day03.filterByCommonBit(diagnosticReport, filterMostCommon = false)

    assert(filteredByMostCommonBit === List("100111101011"))
    assert(filteredByLeastCommonBit === List("001011000101"))

    assert(Day03.binaryToDecimal(filteredByMostCommonBit.head) === 2539)
    assert(Day03.binaryToDecimal(filteredByLeastCommonBit.head) === 709)

    assert(Day03.determineOxygenGeneratorRating(diagnosticReport) === 2539)
    assert(Day03.determineC02ScrubberRating(diagnosticReport) === 709)
    assert(Day03.determineLifesupportRating(diagnosticReport) === 1800151)
  }

}
