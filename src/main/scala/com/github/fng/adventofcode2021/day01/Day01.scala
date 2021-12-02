package com.github.fng.adventofcode2021.day01

object Day01 extends App {
  def numberOfTimesANumberIncreases(input: List[String]): Int = {
    input.zip(input.tail).foldLeft(0) { case (buffer, (current, next)) =>
      if (next > current) buffer + 1
      else buffer
    }
  }

}
