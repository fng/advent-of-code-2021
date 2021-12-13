package com.github.fng.adventofcode2021.day10

import com.github.fng.adventofcode2021.ResourceUtils
import com.github.fng.adventofcode2021.day10.Day10.{Corrupted, Incomplete}
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite {

  test("Day10 - Part1 - reference") {
    val input = ResourceUtils.getLinesFromResource("day10/reference-input.txt")

    val validatedLines =
      input.map(line => line -> Day10.validateLineOfChunks(line))
    assert(
      validatedLines.mkString("\n") === """([({(<(())[]>[[{[]{<()<>>,Incomplete(List({, {, [, [, (, {, (, [)))
                                          |([(()[<>])]({[<{<<[]>>(,Incomplete(List((, {, <, [, {, ()))
                                          |({([(<{}[<>[]}>{[]{[(<()>,Corrupted(}))
                                          |((((({<>}<{<{<>}{[]{[]{},Incomplete(List({, {, <, {, <, (, (, (, ()))
                                          |([[<[([]))<([[{}[[()]]],Corrupted()))
                                          |([{[{({}]{}}([{[{{{}}([],Corrupted(]))
                                          |({<[[]]>}<{[{[{[]{()[[[],Incomplete(List([, [, {, {, [, {, [, {, <)))
                                          |([<(<(<(<{}))><([]([](),Corrupted()))
                                          |(<{([([[(<>()){}]>(<<{{,Corrupted(>))
                                          |(<{([{{}}[<[[[<>{}]]]>[]],Incomplete(List([, (, {, <)))""".stripMargin
    )

    val errorScore = validatedLines
      .map(_._2)
      .collect { case c @ Corrupted(_) =>
        c.errorScore
      }
      .sum

    assert(errorScore === 26397)
  }

  test("Day10 - Part1 - exercise") {
    val input = ResourceUtils.getLinesFromResource("day10/input.txt")
    val validatedLines = input.map(Day10.validateLineOfChunks)
    val errorScore = validatedLines.collect { case c @ Corrupted(_) =>
      c.errorScore
    }.sum

    assert(errorScore === 311949)
  }

  test("Day10 - Part2 - reference") {
    val input = ResourceUtils.getLinesFromResource("day10/reference-input.txt")
    val incompleteLines =
      input.map(Day10.validateLineOfChunks).collect { case i @ Incomplete(_) =>
        (i.stack, i.completedBy, i.autocompleteScore)
      }

    assert(
      incompleteLines.mkString("\n") === """(List({, {, [, [, (, {, (, [),List(}, }, ], ], ), }, ), ]),288957)
                                                |(List((, {, <, [, {, (),List(), }, >, ], }, )),5566)
                                                |(List({, {, <, {, <, (, (, (, (),List(}, }, >, }, >, ), ), ), )),1480781)
                                                |(List([, [, {, {, [, {, [, {, <),List(], ], }, }, ], }, ], }, >),995444)
                                                |(List([, (, {, <),List(], ), }, >),294)""".stripMargin
    )

    assert(Day10.middleScore(incompleteLines.map(_._3)) === 288957)
  }

  test("Day10 - Part2 - exercise") {
    val input = ResourceUtils.getLinesFromResource("day10/input.txt")
    val scores =
      input.map(Day10.validateLineOfChunks).collect { case i @ Incomplete(_) =>
        i.autocompleteScore
      }

    assert(Day10.middleScore(scores) === 3042730309L)
  }

}
