package com.github.fng.adventofcode2021.day07

object Day07 {

  def parseInput(lines: List[String]): List[Int] = {
    assert(lines.length == 1, "Exactly one line as input expected")
    lines.head.split(",").map(_.toInt).toList
  }

  object Part1 {
    def fuelCostToMoveAllTo(currentPositions: List[Int], to: Int): Int = {
      currentPositions.map(currentPosition => (currentPosition - to).abs).sum
    }

    def alignToPositionWithLeastFuel(startPositions: List[Int]): (Int, Int) = {
      val min = startPositions.min
      val max = startPositions.max

      min
        .to(max)
        .map { targetPosition =>
          targetPosition -> fuelCostToMoveAllTo(startPositions, targetPosition)
        }
        .minBy(_._2)
    }
  }

}
