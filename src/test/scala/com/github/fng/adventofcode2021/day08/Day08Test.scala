package com.github.fng.adventofcode2021.day08

import com.github.fng.adventofcode2021.ResourceUtils
import org.scalatest.funsuite.AnyFunSuite

class Day08Test extends AnyFunSuite {

  test("Day08 - Part1 - reference") {

    val input = ResourceUtils.getLinesFromResource("day08/reference-input.txt")
    val parsedInput
        : List[(Day08.UniqueSignalPatterns, Day08.FourDigitOutputValues)] =
      Day08.parseInput(input)

    assert(
      parsedInput.mkString("\n") === """(UniqueSignalPatterns(List(be, cfbegad, cbdgef, fgaecd, cgeb, fdcge, agebfd, fecdb, fabcd, edb)),FourDigitOutputValues(List(fdgacbe, cefdb, cefbgd, gcbe)))
                                            |(UniqueSignalPatterns(List(edbfga, begcd, cbg, gc, gcadebf, fbgde, acbgfd, abcde, gfcbed, gfec)),FourDigitOutputValues(List(fcgedb, cgb, dgebacf, gc)))
                                            |(UniqueSignalPatterns(List(fgaebd, cg, bdaec, gdafb, agbcfd, gdcbef, bgcad, gfac, gcb, cdgabef)),FourDigitOutputValues(List(cg, cg, fdcagb, cbg)))
                                            |(UniqueSignalPatterns(List(fbegcd, cbd, adcefb, dageb, afcb, bc, aefdc, ecdab, fgdeca, fcdbega)),FourDigitOutputValues(List(efabcd, cedba, gadfec, cb)))
                                            |(UniqueSignalPatterns(List(aecbfdg, fbg, gf, bafeg, dbefa, fcge, gcbea, fcaegb, dgceab, fcbdga)),FourDigitOutputValues(List(gecf, egdcabf, bgf, bfgea)))
                                            |(UniqueSignalPatterns(List(fgeab, ca, afcebg, bdacfeg, cfaedg, gcfdb, baec, bfadeg, bafgc, acf)),FourDigitOutputValues(List(gebdcfa, ecba, ca, fadegcb)))
                                            |(UniqueSignalPatterns(List(dbcfg, fgd, bdegcaf, fgec, aegbdf, ecdfab, fbedc, dacgb, gdcebf, gf)),FourDigitOutputValues(List(cefg, dcbef, fcge, gbcadfe)))
                                            |(UniqueSignalPatterns(List(bdfegc, cbegaf, gecbf, dfcage, bdacg, ed, bedf, ced, adcbefg, gebcd)),FourDigitOutputValues(List(ed, bcgafe, cdgba, cbgef)))
                                            |(UniqueSignalPatterns(List(egadfb, cdbfeg, cegd, fecab, cgb, gbdefca, cg, fgcdab, egfdb, bfceg)),FourDigitOutputValues(List(gbdfcae, bgc, cg, cgb)))
                                            |(UniqueSignalPatterns(List(gcafb, gcf, dcaebfg, ecagb, gf, abcdeg, gaef, cafbge, fdbac, fegbdc)),FourDigitOutputValues(List(fgae, cfgab, fg, bagce)))""".stripMargin
    )

    val fourDigitOutputValues = parsedInput.map(_._2)

    assert(
      fourDigitOutputValues
        .map(output => output -> output.countUniqueOutputValues)
        .mkString("\n") ===
        """(FourDigitOutputValues(List(fdgacbe, cefdb, cefbgd, gcbe)),2)
       |(FourDigitOutputValues(List(fcgedb, cgb, dgebacf, gc)),3)
       |(FourDigitOutputValues(List(cg, cg, fdcagb, cbg)),3)
       |(FourDigitOutputValues(List(efabcd, cedba, gadfec, cb)),1)
       |(FourDigitOutputValues(List(gecf, egdcabf, bgf, bfgea)),3)
       |(FourDigitOutputValues(List(gebdcfa, ecba, ca, fadegcb)),4)
       |(FourDigitOutputValues(List(cefg, dcbef, fcge, gbcadfe)),3)
       |(FourDigitOutputValues(List(ed, bcgafe, cdgba, cbgef)),1)
       |(FourDigitOutputValues(List(gbdfcae, bgc, cg, cgb)),4)
       |(FourDigitOutputValues(List(fgae, cfgab, fg, bagce)),2)""".stripMargin
    )

    assert(fourDigitOutputValues.map(_.countUniqueOutputValues()).sum === 26)

  }

  test("Day08 - Part1 - exercise") {
    val input = ResourceUtils.getLinesFromResource("day08/input.txt")
    val fourDigitOutputValues = Day08.parseInput(input).map(_._2)
    assert(fourDigitOutputValues.map(_.countUniqueOutputValues()).sum === 456)
  }

}
