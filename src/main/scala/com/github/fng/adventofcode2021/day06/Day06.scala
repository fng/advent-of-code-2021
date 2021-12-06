package com.github.fng.adventofcode2021.day06

import scala.annotation.tailrec
import scala.collection.mutable

object Day06 {

  def parseInput(lines: List[String]): List[Long] = {
    assert(lines.length == 1, "Exactly one line as input expected")
    lines.head.split(",").map(_.toLong).toList
  }

  def calculationPopulationForNextDay(population: List[Long]): List[Long] = {
    val newAgeAndNewFish = population.map { age =>
      if (age == 0) 6L -> Some(8L)
      else (age - 1) -> None
    }

    newAgeAndNewFish.map(_._1) ++ newAgeAndNewFish.collect {
      case (_, Some(x)) => x
    }
  }

  @tailrec
  def calculationPopulationRecursive(
      population: List[Long],
      daysLeft: Int
  ): List[Long] = {
    val newAgeAndNewFish = population.map { age =>
      if (age == 0) 6L -> Some(8L)
      else (age - 1) -> None
    }
    val newPopulation = newAgeAndNewFish.map(_._1) ++ newAgeAndNewFish.collect {
      case (_, Some(x)) => x
    }

    println(s"$daysLeft -> ${newPopulation.length}")

    if (daysLeft - 1 > 0)
      calculationPopulationRecursive(newPopulation, daysLeft - 1)
    else newPopulation
  }

  def populationToMap(population: List[Long]): Map[Int, Long] =
    population.groupBy(a => a.toInt).view.mapValues(_.length.toLong).toMap

  def calculatePopulationWithMap(map: Map[Int, Long]): Map[Int, Long] = {
    val newMap = 1
      .to(8)
      .map { age =>
        (age - 1) -> map.getOrElse(age, 0L)
      }
      .toMap

    val fishToSpawn = map.getOrElse(0, 0L)

    newMap
      .updated(6, newMap.getOrElse(6, 0L) + fishToSpawn)
      .updated(8, fishToSpawn)
  }

}
