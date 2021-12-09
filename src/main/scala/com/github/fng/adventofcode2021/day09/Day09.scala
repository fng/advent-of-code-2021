package com.github.fng.adventofcode2021.day09

object Day09 {

  def parseInputToMap(input: List[String]): Map[(Int, Int), Int] = {
    input.zipWithIndex.flatMap { case (line, y) =>
      line.toCharArray.toList.map(_.toString.toInt).zipWithIndex.map {
        case (height, x) =>
          (x, y) -> height
      }
    }.toMap
  }

  def lowestPoints(map: Map[(Int, Int), Int]): List[((Int, Int), Int)] = {
    map.toList.filterNot { case ((x, y), height) =>
      val up = map.get((x, y - 1))
      val isUpLowerOrEqual = up.exists(up => up <= height)
      val down = map.get((x, y + 1))
      val isDownLowerOrEqual = down.exists(down => down <= height)
      val left = map.get((x - 1, y))
      val isLeftLowerOrEqual = left.exists(left => left <= height)
      val right = map.get((x + 1, y))
      val isRightLowerOrEqual = right.exists(right => right <= height)

      val isNotLowPoint =
        isUpLowerOrEqual || isDownLowerOrEqual || isLeftLowerOrEqual || isRightLowerOrEqual
      if (!isNotLowPoint) {

        println(s"""-$up-
                     |$left$height$right
                     |-$down-""".stripMargin)

        println("here")
      }

      isNotLowPoint
    }
  }

  def calculateRiskLevel(map: Map[(Int, Int), Int]): Int = {
    lowestPoints(map).map(_._2 + 1).sum
  }

}
