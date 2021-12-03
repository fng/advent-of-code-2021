package com.github.fng.adventofcode2021.day03

object Day03 {

  def mostCommonBitByIndex(binaryNumbers: List[String]): String = {
    0.until(binaryNumbers.head.length)
      .map { index =>
        val bitAtIndex = binaryNumbers.map(_.substring(index, index + 1))
        val numberOf0Bits = bitAtIndex.count(_ == "0")
        val numberOf1Bits = bitAtIndex.count(_ == "1")
        if (numberOf0Bits > numberOf1Bits) "0"
        else "1"
      }
      .mkString
  }

  def inverseCommonBitByIndex(binaryNumber: String): String = {
    binaryNumber
      .map(_.toString match {
        case "0" => "1"
        case "1" => "0"
      })
      .mkString
  }

  def binaryToDecimal(binaryNumber: String): Int = {
    binaryNumber.reverse.zipWithIndex
      .map { case (bit, index) =>
        val result = charBitToInt(bit) * scala.math.pow(2, index)
        result
      }
      .sum
      .toInt
  }

  def charBitToInt(bit: Char): Int = bit.toString match {
    case "0" => 0
    case "1" => 1
  }

  def calculatePowerConsumption(diagnosticReport: List[String]): Int = {
    val gammaRateBinary = mostCommonBitByIndex(diagnosticReport)
    val epsilonRateBinary = inverseCommonBitByIndex(gammaRateBinary)
    val gammaRate = binaryToDecimal(gammaRateBinary)
    val epsilonRate = binaryToDecimal(epsilonRateBinary)

    gammaRate * epsilonRate
  }

  def filterByCommonBit(
      binaryNumbers: List[String],
      filterMostCommon: Boolean
  ): List[String] = {
    0.until(binaryNumbers.head.length).foldLeft(binaryNumbers) {
      case (remainingBinaryNumbers, filterBitIndex) =>
        //to not filter further if there is only one number left
        if (remainingBinaryNumbers.length == 1) {
          remainingBinaryNumbers
        } else {
          val bitsAtIndex = remainingBinaryNumbers
            .map(_.substring(filterBitIndex, filterBitIndex + 1))
          val numberOf0Bits = bitsAtIndex.count(_ == "0")
          val numberOf1Bits = bitsAtIndex.count(_ == "1")

          val commonBit = if (filterMostCommon) {
            if (numberOf0Bits > numberOf1Bits) "0"
            else if (numberOf0Bits == numberOf1Bits) "1"
            else "1"
          } else {
            if (numberOf0Bits < numberOf1Bits) "0"
            else if (numberOf0Bits == numberOf1Bits) "0"
            else "1"
          }

          remainingBinaryNumbers.filter(
            _.substring(filterBitIndex, filterBitIndex + 1) == commonBit
          )
        }

    }
  }

  def determineOxygenGeneratorRating(binaryNumbers: List[String]): Int =
    binaryToDecimal(
      filterByCommonBit(binaryNumbers, filterMostCommon = true).head
    )

  def determineC02ScrubberRating(binaryNumbers: List[String]): Int =
    binaryToDecimal(
      filterByCommonBit(binaryNumbers, filterMostCommon = false).head
    )

  def determineLifesupportRating(diagnosticReport: List[String]): Int =
    determineOxygenGeneratorRating(
      diagnosticReport
    ) * determineC02ScrubberRating(diagnosticReport)

}
