package com.github.fng.adventofcode2021.day08

import com.github.fng.adventofcode2021.day08.Day08.FourDigitOutputValues.outputWithUniqueSegments

object Day08 {

  case class UniqueSignalPatterns(patterns: List[String])

  object FourDigitOutputValues {
    val outputWithUniqueSegments = List(
      2, //1 -> 2 segments
      4, //4 -> 4 segments
      3, //7 -> 3 segments
      7 //8 -> 7 segments
    )
  }

  case class FourDigitOutputValues(digitOutputValue: List[String]) {
    def countUniqueOutputValues(): Int = {
      digitOutputValue
        .map(_.length)
        .count(length => outputWithUniqueSegments.contains(length))
    }
  }

  def parseInput(
      lines: List[String]
  ): List[(UniqueSignalPatterns, FourDigitOutputValues)] = {
    lines.map { line =>
      line.split(" \\| ").toList match {
        case List(patternsString, outputValueString) =>
          (
            UniqueSignalPatterns(patternsString.split(" ").toList),
            FourDigitOutputValues(outputValueString.split(" ").toList)
          )
        case other => sys.error(s"Can't match input line: $other")
      }
    }
  }

}
