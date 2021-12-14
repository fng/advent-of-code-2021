package com.github.fng.adventofcode2021.day14

import com.github.fng.adventofcode2021.ResourceUtils
import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite {

  test("Day14 - Part1 - reference") {
    val input = ResourceUtils.getLinesFromResource("day14/reference-input.txt")
    val (template, rules) = Day14.parseInput(input)

    assert(template.polymer === "NNCB")
    assert(rules.map.toList.mkString("\n") === """(BB,N)
                                                        |(HH,N)
                                                        |(HC,B)
                                                        |(BN,B)
                                                        |(BC,B)
                                                        |(CN,C)
                                                        |(BH,H)
                                                        |(NN,C)
                                                        |(CC,N)
                                                        |(CH,B)
                                                        |(HB,C)
                                                        |(CB,H)
                                                        |(HN,C)
                                                        |(NH,C)
                                                        |(NC,B)
                                                        |(NB,B)""".stripMargin)

    assert(template.process(rules).polymer === "NCNBCHB")
    assert(template.processNTimes(rules, 1).polymer === "NCNBCHB")
    assert(template.processNTimes(rules, 2).polymer === "NBCCNBBBCBHCB")
    assert(
      template.processNTimes(rules, 3).polymer === "NBBBCNCCNBBNBNBBCHBHHBCHB"
    )
    assert(
      template
        .processNTimes(rules, 4)
        .polymer === "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
    )

    val after10Times = template.processNTimes(rules, 10)
    assert(
      after10Times.countElements.mkString("\n") === """(B,1749)
                                                           |(C,298)
                                                           |(H,161)
                                                           |(N,865)""".stripMargin
    )
    assert(after10Times.score === 1588)
  }

  test("Day14 - Part1 - exercise") {
    val input = ResourceUtils.getLinesFromResource("day14/input.txt")
    val (template, rules) = Day14.parseInput(input)
    val after10Times = template.processNTimes(rules, 10)
    assert(after10Times.score === 3247)
  }

}
