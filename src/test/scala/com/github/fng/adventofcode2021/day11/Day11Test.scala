package com.github.fng.adventofcode2021.day11

import com.github.fng.adventofcode2021.ResourceUtils
import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends AnyFunSuite {

  test("Day11 - Part1 - reference") {
    val input = ResourceUtils.getLinesFromResource("day11/reference-input.txt")
    val beforeAnySteps = Day11.parseGrid(input)
    assertString(beforeAnySteps.toString,
      """5483143223
        |2745854711
        |5264556173
        |6141336146
        |6357385478
        |4167524645
        |2176841721
        |6882881134
        |4846848554
        |5283751526""".stripMargin)


    assertString(Day11.parseGrid(input).runSteps(1).toString,
      """6594254334
        |3856965822
        |6375667284
        |7252447257
        |7468496589
        |5278635756
        |3287952832
        |7993992245
        |5957959665
        |6394862637""".stripMargin)

    assertString(Day11.parseGrid(input).runSteps(2).toString,
      """8807476555
        |5089087054
        |8597889608
        |8485769600
        |8700908800
        |6600088989
        |6800005943
        |0000007456
        |9000000876
        |8700006848""".stripMargin)

    assertString(Day11.parseGrid(input).runSteps(10).toString,
      """0481112976
        |0031112009
        |0041112504
        |0081111406
        |0099111306
        |0093511233
        |0442361130
        |5532252350
        |0532250600
        |0032240000""".stripMargin)

    val after100Steps = Day11.parseGrid(input).runSteps(100)
    assertString(after100Steps.toString,
      """0397666866
        |0749766918
        |0053976933
        |0004297822
        |0004229892
        |0053222877
        |0532222966
        |9322228966
        |7922286866
        |6789998766""".stripMargin)

    assert(after100Steps.flashes === 1656)

  }

    test("Day11 - Part1 - exercise") {
      val input = ResourceUtils.getLinesFromResource("day11/input.txt")
      val after100Steps = Day11.parseGrid(input).runSteps(100)
      assert(after100Steps.flashes === 1546)
    }

  test("Day11 - Part2 - reference") {
    val input = ResourceUtils.getLinesFromResource("day11/reference-input.txt")
    val initialGrid = Day11.parseGrid(input)
    assert(initialGrid.flashAllAfterStep(1) === 195)
  }

  test("Day11 - Part2 - exercies") {
    val input = ResourceUtils.getLinesFromResource("day11/input.txt")
    val initialGrid = Day11.parseGrid(input)
    assert(initialGrid.flashAllAfterStep(1) === 471)
  }


  private def assertString(actual: String, expected: String): Unit = {
    assert(actual.replace("\r\n", "\n") ===
      expected.replace("\r\n", "\n").stripMargin)
  }

}
