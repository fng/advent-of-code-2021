package com.github.fng.adventofcode2021.day09

import scala.collection.mutable

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
      isNotLowPoint
    }
  }

  def calculateRiskLevel(map: Map[(Int, Int), Int]): Int = {
    lowestPoints(map).map(_._2 + 1).sum
  }

  def basinForLowPoint(
      map: Map[(Int, Int), Int],
      lowPointCoordinate: (Int, Int)
  ): Map[(Int, Int), Int] = {

    //start at low point and move into all directions.
    //add point to basin if height is less than 9
    //move out to all directions from the new point.

    val visitedMap: mutable.Map[(Int, Int), Int] = mutable.Map()

    def visit(coordinate: (Int, Int)): Unit = {
      if (visitedMap.contains(coordinate)) {
        //already visited
      } else {
        map.get(coordinate).foreach { height =>
          visitedMap += (coordinate -> height)

          if (height < 9) {
            val (x, y) = coordinate
            //visit up
            visit((x, y - 1))
            //visit down
            visit((x, y + 1))
            //visit left
            visit((x - 1, y))
            //visit right
            visit((x + 1, y))
          }
        }
      }
    }

    visit(lowPointCoordinate)

    visitedMap.filterNot(_._2 == 9).toMap

  }

  def calculateLargestBasinsProduct(
      map: Map[(Int, Int), Int],
      lowPoints: List[(Int, Int)]
  ): Int = {
    lowPoints
      .map(lowPoint => Day09.basinForLowPoint(map, lowPoint).size)
      .sorted
      .reverse
      .take(3)
      .product
  }

}
