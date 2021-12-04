package com.github.fng.adventofcode2021.day04

import com.github.fng.adventofcode2021.ResourceUtils
import com.github.fng.adventofcode2021.day04.Day04.{Board, DrawnNumbers}
import org.scalatest.funsuite.AnyFunSuite

class Day04Test extends AnyFunSuite {

  test("Day04 - Parse") {

    val input = ResourceUtils.getLinesFromResource("day04/reference-input.txt")

    val (drawNumbers, boards) = Day04.parseInput(input)

    assert(
      drawNumbers === DrawnNumbers(
        List(7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25,
          12, 22, 18, 20, 8, 19, 3, 26, 1)
      )
    )

    assert(
      boards.mkString("\n\n") ===
        """22 13 17 11  0
        | 8  2 23  4 24
        |21  9 14 16  7
        | 6 10  3 18  5
        | 1 12 20 15 19
        |
        | 3 15  0  2 22
        | 9 18 13 17  5
        |19  8  7 25 23
        |20 11 10 24  4
        |14 21 16 12  6
        |
        |14 21 17 24  4
        |10 16 15  9 19
        |18  8 23 26 20
        |22 11 13  6  5
        | 2  0 12  3  7""".stripMargin
    )
  }

  test("Day04 - Part 1 - reference") {

    val input = ResourceUtils.getLinesFromResource("day04/reference-input.txt")

    val (drawNumbers, initialBoards) = Day04.parseInput(input)

    val (drawnNumbersUntilBingo, finalBoards) =
      drawNumbers.numbers.foldLeft[(DrawnNumbers, List[Board])](
        (DrawnNumbers(Nil), initialBoards)
      ) { case ((drawnNumbers, boards), number) =>
        if (!boards.exists(_.hasBingo)) {
          val newDrawnNumbers = drawnNumbers.draw(number)
          val newBoards = boards.map(_.draw(number))
          (newDrawnNumbers, newBoards)
        } else (drawnNumbers, boards)
      }

    assert(
      drawnNumbersUntilBingo === DrawnNumbers(
        List(7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24)
      )
    )

    assert(
      finalBoards.mkString("\n\n") ===
        """22 13 <17> <11> < 0>
        | 8 < 2> <23> < 4> <24>
        |<21> < 9> <14> 16 < 7>
        | 6 10  3 18 < 5>
        | 1 12 20 15 19
        |
        | 3 15 < 0> < 2> 22
        |< 9> 18 13 <17> < 5>
        |19  8 < 7> 25 <23>
        |20 <11> 10 <24> < 4>
        |<14> <21> 16 12  6
        |
        |<14> <21> <17> <24> < 4>
        |10 16 15 < 9> 19
        |18  8 <23> 26 20
        |22 <11> 13  6 < 5>
        |< 2> < 0> 12  3 < 7>""".stripMargin
    )

    assert(
      finalBoards.map(
        _.calculateScore(drawnNumbersUntilBingo.numbers.last)
      ) === List(None, None, Some(4512))
    )

  }

  test("Day04 - Part 1 - exercise") {
    val input = ResourceUtils.getLinesFromResource("day04/input.txt")

    val (drawNumbers, initialBoards) = Day04.parseInput(input)

    assert(drawNumbers.numbers.length === 100)
    assert(initialBoards.length === 100)

    val (drawnNumbersUntilBingo, finalBoards) =
      drawNumbers.numbers.foldLeft[(DrawnNumbers, List[Board])](
        (DrawnNumbers(Nil), initialBoards)
      ) { case ((drawnNumbers, boards), number) =>
        if (!boards.exists(_.hasBingo)) {
          val newDrawnNumbers = drawnNumbers.draw(number)
          val newBoards = boards.map(_.draw(number))
          (newDrawnNumbers, newBoards)
        } else (drawnNumbers, boards)
      }

    assert(
      drawnNumbersUntilBingo === DrawnNumbers(
        List(73, 42, 95, 35, 13, 40, 99, 92, 33, 30, 83, 1, 36, 93, 59, 90, 55,
          25, 77, 44, 37, 62, 41, 47, 80, 23, 51, 61, 21, 20, 76, 8, 71)
      )
    )

    assert(
      finalBoards
        .find(_.hasBingo)
        .flatMap(
          _.calculateScore(drawnNumbersUntilBingo.numbers.last)
        ) === Some(39902)
    )

  }

}
