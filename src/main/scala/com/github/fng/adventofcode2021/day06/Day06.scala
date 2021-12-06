package com.github.fng.adventofcode2021.day06

object Day06 {

  def parseInput(lines: List[String]): List[Int] = {
    assert(lines.length == 1, "Exactly one line as input expected")
    lines.head.split(",").map(_.toInt).toList
  }

  def calculationPopulationForNextDay(population: List[Int]): List[Int] = {
    val newAgeAndNewFish = population.map { age =>
      if (age == 0) 6 -> Some(8)
      else (age - 1) -> None
    }

    newAgeAndNewFish.map(_._1) ++ newAgeAndNewFish.collect {
      case (_, Some(x)) => x
    }
  }

}
