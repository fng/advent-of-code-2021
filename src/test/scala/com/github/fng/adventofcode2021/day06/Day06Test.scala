package com.github.fng.adventofcode2021.day06

import com.github.fng.adventofcode2021.ResourceUtils
import com.github.fng.adventofcode2021.day05.Day05.Point
import org.scalatest.funsuite.AnyFunSuite

class Day06Test extends AnyFunSuite {

  test("Day06 - Part1 - reference") {

    val input = ResourceUtils.getLinesFromResource("day06/reference-input.txt")

    val initialPopulation = Day06.parseInput(input)

    assert(initialPopulation === List(3, 4, 3, 1, 2))

    println(s"Initial state: ${initialPopulation.mkString(",")}")

    val populationAfter18Days =
      1.to(18).foldLeft(initialPopulation) { case (population, day) =>
        val newPopulation = Day06.calculationPopulationForNextDay(population)
        println(
          s"After $day day: ${newPopulation.mkString(",")} -> ${newPopulation.length}"
        )
        newPopulation
      }

    assert(populationAfter18Days.length === 26)

    val populationAfter80days = 1.to(80).foldLeft(initialPopulation) {
      case (population, _) => Day06.calculationPopulationForNextDay(population)
    }

    assert(populationAfter80days.length === 5934)
  }

  test("Day06 - Part1 - exercise") {

    val input = ResourceUtils.getLinesFromResource("day06/input.txt")
    val initialPopulation = Day06.parseInput(input)
    val populationAfter80days = 1.to(80).foldLeft(initialPopulation) {
      case (population, _) => Day06.calculationPopulationForNextDay(population)
    }

    assert(populationAfter80days.length === 379414)
  }

}
