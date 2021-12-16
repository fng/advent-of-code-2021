package com.github.fng.adventofcode2021.day16

import com.github.fng.adventofcode2021.ResourceUtils
import com.github.fng.adventofcode2021.day16.Day16.{LiteralPackage, Operator}
import org.scalatest.funsuite.AnyFunSuite

class Day16Test extends AnyFunSuite {

  test("Day16 - hexStringToBinary") {
    assert(Day16.hexStringToBinary("D2FE28") === "110100101111111000101000")
    assert(
      Day16.hexStringToBinary(
        "38006F45291200"
      ) === "00111000000000000110111101000101001010010001001000000000"
    )
    assert(
      Day16.hexStringToBinary(
        "EE00D40C823060"
      ) === "11101110000000001101010000001100100000100011000001100000"
    )
  }

  test("Day16 - literal value") {
    val parsed = Day16.parseHex("D2FE28")
    assert(parsed === (LiteralPackage(6, 4, "011111100101", 2021), "000"))
  }

  test("Day16 - type 0 operator") {
    val parsed = Day16.parseHex("38006F45291200")
    assert(
      parsed === (Operator(
        1,
        6,
        0,
        List(
          LiteralPackage(6, 4, "1010", 10),
          LiteralPackage(2, 4, "00010100", 20)
        )
      ), "0000000")
    )
  }

  test("Day16 - type 1 operator") {
    val parsed = Day16.parseHex("EE00D40C823060")
    assert(
      parsed === (Operator(
        7,
        3,
        1,
        List(
          LiteralPackage(2, 4, "0001", 1),
          LiteralPackage(4, 4, "0010", 2),
          LiteralPackage(1, 4, "0011", 3)
        )
      ), "00000")
    )
  }

  test("Day16 - example 1") {
    val parsed = Day16.parseHex("8A004A801A8002F478")
    assert(
      parsed === (Operator(
        4,
        2,
        1,
        List(
          Operator(
            1,
            2,
            1,
            List(Operator(5, 2, 0, List(LiteralPackage(6, 4, "1111", 15))))
          )
        )
      ), "000")
    )
    assert(parsed._1.versionSum === 16)
  }

  test("Day16 - example 2") {
    val parsed = Day16.parseHex("620080001611562C8802118E34")
    assert(
      parsed === (Operator(
        3,
        0,
        1,
        List(
          Operator(
            0,
            0,
            0,
            List(
              LiteralPackage(0, 4, "1010", 10),
              LiteralPackage(5, 4, "1011", 11)
            )
          ),
          Operator(
            1,
            0,
            1,
            List(
              LiteralPackage(0, 4, "1100", 12),
              LiteralPackage(3, 4, "1101", 13)
            )
          )
        )
      ), "00")
    )
    assert(parsed._1.versionSum === 12)
  }

  test("Day16 - example 3") {
    val parsed = Day16.parseHex("C0015000016115A2E0802F182340")
    assert(
      parsed === (Operator(
        6,
        0,
        0,
        List(
          Operator(
            0,
            0,
            0,
            List(
              LiteralPackage(0, 4, "1010", 10),
              LiteralPackage(6, 4, "1011", 11)
            )
          ),
          Operator(
            4,
            0,
            1,
            List(
              LiteralPackage(7, 4, "1100", 12),
              LiteralPackage(0, 4, "1101", 13)
            )
          )
        )
      ), "000000")
    )
    assert(parsed._1.versionSum === 23)
  }

  test("Day16 - example 4") {
    val parsed = Day16.parseHex("A0016C880162017C3686B18A3D4780")
    assert(
      parsed === (Operator(
        5,
        0,
        0,
        List(
          Operator(
            1,
            0,
            1,
            List(
              Operator(
                3,
                0,
                1,
                List(
                  LiteralPackage(7, 4, "0110", 6),
                  LiteralPackage(6, 4, "0110", 6),
                  LiteralPackage(5, 4, "1100", 12),
                  LiteralPackage(2, 4, "1111", 15),
                  LiteralPackage(2, 4, "1111", 15)
                )
              )
            )
          )
        )
      ), "0000000")
    )
    assert(parsed._1.versionSum === 31)
  }

  test("Day16 - Part1 - exercise") {
    val input = ResourceUtils.getLinesFromResource("day16/input.txt").head
    val parsed = Day16.parseHex(input)
    println(parsed)
    assert(parsed._1.versionSum === 847)
  }

}
