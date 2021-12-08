package com.github.fng.adventofcode2021.day08

import com.github.fng.adventofcode2021.day08.Day08.FourDigitOutputValues.outputWithUniqueSegments

object Day08 {

  case class UniqueSignalPatterns(numbers: List[String]) {

    def decode(): List[(String, Int)] = {

      /*
      Step 1
       - remove only number with 2 segments => 1
       - remove only number with 4 segments => 4
       - remove only number with 3 segments => 7
       - remove only number with 7 segments => 8
       => 0, 2, 3, 5, 6, 9 remain
       */
      val afterStep1 = Map(
        1 -> numbers
          .find(_.length == 2)
          .getOrElse(sys.error("No number found with 2 segments")),
        4 -> numbers
          .find(_.length == 4)
          .getOrElse(sys.error("No number found with 4 segments")),
        7 -> numbers
          .find(_.length == 3)
          .getOrElse(sys.error("No number found with 3 segments")),
        8 -> numbers
          .find(_.length == 7)
          .getOrElse(sys.error("No number found with 7 segments"))
      )
      println(s"after Step1: $afterStep1")

      val remainingAfterStep1 =
        numbers.filterNot(number => afterStep1.values.toList.contains(number))
      println(s"remaining after Step1: $remainingAfterStep1")

      /*
      Step 2
        segments of 4 + 7 (distinct) are contained in one number with one additional one gggg -> 9
        => 0, 2, 3, 5, 6 remain
       */
      val _4plus7Segments = (numberToSegments(
        afterStep1(4)
      ) ++ numberToSegments(afterStep1(7))).distinct
      val number9 = remainingAfterStep1
        .find { number =>
          val segments = numberToSegments(number)
          _4plus7Segments.forall(segments.contains(_)) && segments
            .diff(_4plus7Segments)
            .length == 1
        }
        .getOrElse(sys.error("Number 9 not found"))

      val afterStep2 = afterStep1 + (9 -> number9)
      println(s"after Step2: $afterStep2")
      val remainingAfterStep2 =
        numbers.filterNot(number => afterStep2.values.toList.contains(number))
      println(s"remaining after Step2: $remainingAfterStep2")

      /*
      Step 3
        must contain segments of 1 and has 6 segments => 0
        => 2, 3, 5 ,6 remain
       */
      val _1segments = numberToSegments(afterStep2(1))
      val remainingAfterStep2Containing1Segments =
        remainingAfterStep2.filter(number =>
          _1segments.forall(numberToSegments(number).contains(_))
        )

      val number0 = remainingAfterStep2Containing1Segments
        .find(_.length == 6)
        .getOrElse(sys.error("Number 0 not found"))

      val afterStep3 = afterStep2 + (0 -> number0)
      println(s"after Step3: $afterStep3")
      val remainingAfterStep3 =
        numbers.filterNot(number => afterStep3.values.toList.contains(number))
      println(s"remaining after Step3: $remainingAfterStep3")

      /*
      Step 4
        only one number is missing a segment which is contained in all other numbers (ff) -> 2
        => 3, 5, 6 remain
       */

      val distinctSegments = remainingAfterStep3
        .flatMap(_.toCharArray.toList.map(_.toString))
        .distinct
      val missingSegmentInNumber2 = distinctSegments
        .map(segment =>
          segment -> remainingAfterStep3.count(number =>
            number.contains(segment)
          )
        )
        .find(_._2 == 3)
        .getOrElse(sys.error("Missing segment in number 2 not found"))
        ._1
      val number2 = remainingAfterStep3
        .find(number =>
          !numberToSegments(number).contains(missingSegmentInNumber2)
        )
        .getOrElse(sys.error("Number 0 not found"))

      val afterStep4 = afterStep3 + (2 -> number2)
      println(s"after Step4: $afterStep4")
      val remainingAfterStep4 =
        numbers.filterNot(number => afterStep4.values.toList.contains(number))
      println(s"remaining after Step4: $remainingAfterStep4")

      /*
      Step 5
        only one number has 6 segments -> 6
        => 3, 5 remain
       */
      val number6 = remainingAfterStep4
        .find(_.length == 6)
        .getOrElse(sys.error("Number 6 not found"))

      val afterStep5 = afterStep4 + (6 -> number6)
      println(s"after Step45: $afterStep5")
      val remainingAfterStep5 =
        numbers.filterNot(number => afterStep5.values.toList.contains(number))
      println(s"remaining after Step5: $remainingAfterStep5")

      /*
      Step 6
        number 3 contains all segments from number 1 -> 3
        remain -> 5
       */

      val number3 = remainingAfterStep5
        .find(number => _1segments.forall(numberToSegments(number).contains(_)))
        .getOrElse(sys.error("Number 3 not found"))

      val number5 = remainingAfterStep5
        .find(_ != number3)
        .getOrElse(sys.error("Number 5 not found"))

      val finalMap = (afterStep5 + (3 -> number3)) + (5 -> number5)
      finalMap.toList.map(p => p._2 -> p._1)
    }

  }

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

    def findNumberInEncoding(
        encoding: List[(String, Int)],
        number: String
    ): Int = {
      encoding
        .find { case (num, _) =>
          numberToSegments(num).diff(numberToSegments(number)).isEmpty &&
            numberToSegments(number).diff(numberToSegments(num)).isEmpty
        }
        .getOrElse(sys.error(s"number $number not found in encoding"))
        ._2
    }

    def decode(encoding: List[(String, Int)]): Int = {
      digitOutputValue
        .map { number =>
          findNumberInEncoding(encoding, number)
        }
        .mkString
        .toInt
    }
  }

  private def numberToSegments(number: String): List[String] =
    number.toCharArray.toList.map(_.toString)

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
