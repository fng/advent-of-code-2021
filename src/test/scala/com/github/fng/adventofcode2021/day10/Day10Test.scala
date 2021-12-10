package com.github.fng.adventofcode2021.day10

import com.github.fng.adventofcode2021.ResourceUtils
import com.github.fng.adventofcode2021.day10.Day10.Corrupted
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite {

  test("Day10 - Part1 - reference") {
    val input = ResourceUtils.getLinesFromResource("day10/reference-input.txt")

    val validatedLines =
      input.map(line => line -> Day10.validateLineOfChunks(line))

    assert(
      validatedLines.mkString("\n") === """([({(<(())[]>[[{[]{<()<>>,Incomplete)
                                               |([(()[<>])]({[<{<<[]>>(,Incomplete)
                                               |({([(<{}[<>[]}>{[]{[(<()>,Corrupted(}))
                                               |((((({<>}<{<{<>}{[]{[]{},Incomplete)
                                               |([[<[([]))<([[{}[[()]]],Corrupted()))
                                               |([{[{({}]{}}([{[{{{}}([],Corrupted(]))
                                               |({<[[]]>}<{[{[{[]{()[[[],Incomplete)
                                               |([<(<(<(<{}))><([]([](),Corrupted()))
                                               |(<{([([[(<>()){}]>(<<{{,Corrupted(>))
                                               |(<{([{{}}[<[[[<>{}]]]>[]],Incomplete)""".stripMargin
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

  }

  test("Day10 - Part2 - exercise") {

    val input = ResourceUtils.getLinesFromResource("day10/input.txt")

  }

}
