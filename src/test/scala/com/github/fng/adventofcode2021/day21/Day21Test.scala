package com.github.fng.adventofcode2021.day21

import com.github.fng.adventofcode2021.ResourceUtils
import com.github.fng.adventofcode2021.day17.Day17.{
  Point,
  TargetArea,
  TargetComputer,
  Velocity
}
import com.github.fng.adventofcode2021.day21.Day21.{Board, DeterministicDice}
import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends AnyFunSuite {

  test("Day21 - Part 1 - reference") {
    val board = Board
      .newBoard(4, 8, new DeterministicDice)
      .playTillWin()
    assert(board.gameScore === Some(739785L))
  }

  test("Day21 - Part 1 - exercise") {
    val board = Board
      .newBoard(6, 10, new DeterministicDice)
      .playTillWin()
    assert(board.gameScore === Some(853776L))
  }

  private def assertString(actual: Any, expected: String): Unit = {
    assert(
      actual.toString.replace("\r\n", "\n") ===
        expected.replace("\r\n", "\n").stripMargin
    )
  }

}
