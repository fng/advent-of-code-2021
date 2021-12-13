package com.github.fng.adventofcode2021.day13

import com.github.fng.adventofcode2021.ResourceUtils
import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite {

  test("Day13 - Part1 - reference") {
    val input = ResourceUtils.getLinesFromResource("day13/reference-input.txt")
    val instruction = Day13.parseInstructions(input)

    assert(instruction.toPaperWithHoles === """...#..#..#.
                                              |....#......
                                              |...........
                                              |#..........
                                              |...#....#.#
                                              |...........
                                              |...........
                                              |...........
                                              |...........
                                              |...........
                                              |.#....#.##.
                                              |....#......
                                              |......#...#
                                              |#..........
                                              |#.#........""".stripMargin)

    val afterOneFold = instruction.foldOnce
    assert(afterOneFold.toPaperWithHoles === """#.##..#..#.
                                               |#...#......
                                               |......#...#
                                               |#...#......
                                               |.#.#..#.###
                                               |...........
                                               |...........""".stripMargin)
    assert(afterOneFold.holes.length === 17)

    val afterTwoFolds = afterOneFold.foldOnce
    assert(afterTwoFolds.toPaperWithHoles === """#####
                                                |#...#
                                                |#...#
                                                |#...#
                                                |#####
                                                |.....
                                                |.....""".stripMargin)

  }

  test("Day13 - Part1 - exercise") {
    val input = ResourceUtils.getLinesFromResource("day13/input.txt")
    val instruction = Day13.parseInstructions(input)
    val afterOneFold = instruction.foldOnce
    assert(afterOneFold.holes.length === 781)
  }

  test("Day13 - Part2 - reference") {
    val input = ResourceUtils.getLinesFromResource("day13/reference-input.txt")
    val instruction = Day13.parseInstructions(input)

    val foldAll = instruction.foldAll
    assert(foldAll.toPaperWithHoles === """#####
                                                |#...#
                                                |#...#
                                                |#...#
                                                |#####
                                                |.....
                                                |.....""".stripMargin)

  }

  test("Day13 - Part2 - exercise") {
    val input = ResourceUtils.getLinesFromResource("day13/input.txt")
    val instruction = Day13.parseInstructions(input)
    val foldAll = instruction.foldAll
    assert(foldAll.holes.length === 99)

    println(foldAll.toPaperWithHoles)
    assert(
      foldAll.toPaperWithHoles ===
        """###..####.###...##...##....##.###..###..
      |#..#.#....#..#.#..#.#..#....#.#..#.#..#.
      |#..#.###..#..#.#....#.......#.#..#.###..
      |###..#....###..#....#.##....#.###..#..#.
      |#....#....#.#..#..#.#..#.#..#.#....#..#.
      |#....####.#..#..##...###..##..#....###..""".stripMargin
    )

  }

}
