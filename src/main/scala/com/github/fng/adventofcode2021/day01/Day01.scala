package com.github.fng.adventofcode2021.day01

object Day01 {

  object Part1 {
    def numberOfTimesANumberIncreases(input: List[Int]): Int = {
      input.zip(input.tail).count { case (current, next) =>
        next > current
      }
    }
  }

  object Part2 {
    def numberOfTimesANumberIncreasesWithSlidingWindow(
        input: List[Int],
        windowSize: Int = 3
    ): Int = {
      Part1.numberOfTimesANumberIncreases(
        input.sliding(windowSize).map(_.sum).toList
      )
    }
  }

}
