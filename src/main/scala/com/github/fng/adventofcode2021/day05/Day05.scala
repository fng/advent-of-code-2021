package com.github.fng.adventofcode2021.day05

object Day05 {

  case class Point(x: Int, y: Int)

  case class Diagram(map: Map[Point, Int]) {
    override def toString: String = {

      val maxX = map.keys.maxBy(_.x)
      val maxY = map.keys.maxBy(_.y)
      val matrix = 0.to(maxY.y).toList.map { y =>
        0.to(maxX.x).toList.map { x =>
          map.get(Point(x, y)).map(_.toString).getOrElse(".")
        }
      }
      matrix.map(_.mkString).mkString("\n")
    }

    def score: Int = {
      map.values.count(_ > 1)
    }

  }

  def parseInput(input: List[String]): List[((Point, Point))] = {
    input.map { line =>
      line.split(" -> ").toList match {
        case List(a, b) => (parsePoint(a), parsePoint(b))
        case other      => sys.error(s"can't parse line: $line")
      }
    }
  }

  private def parsePoint(point: String): Point = point.split(",").toList match {
    case List(x, y) => Point(x.toInt, y.toInt)
    case other      => sys.error(s"Can't parse point: $point")
  }

  object Part1 {
    def generateLinePoints(from: Point, to: Point): List[Point] = {
      if (from.x == to.x) {
        //vertical line
        if (from.y < to.y) from.y.to(to.y).map(y => from.copy(y = y)).toList
        else to.y.to(from.y).map(y => from.copy(y = y)).toList
      } else if (from.y == to.y) {
        //horizontal line
        if (from.x < to.x) from.x.to(to.x).map(x => from.copy(x = x)).toList
        else to.x.to(from.x).map(x => from.copy(x = x)).toList
      } else Nil
    }

    def generateDiagram(fromToPoints: List[(Point, Point)]): Diagram = {
      val map = fromToPoints
        .flatMap(fromTo => generateLinePoints(fromTo._1, fromTo._2))
        .foldLeft(Map[Point, Int]()) { case (map, point) =>
          map.updatedWith(point) { old => old.map(_ + 1).orElse(Some(1)) }
        }
      Diagram(map)
    }
  }

  object Part2 {
    def generateLinePoints(from: Point, to: Point): List[Point] = {
      if (from.x == to.x) {
        //vertical line
        if (from.y < to.y) from.y.to(to.y).map(y => from.copy(y = y)).toList
        else to.y.to(from.y).map(y => from.copy(y = y)).toList
      } else if (from.y == to.y) {
        //horizontal line
        if (from.x < to.x) from.x.to(to.x).map(x => from.copy(x = x)).toList
        else to.x.to(from.x).map(x => from.copy(x = x)).toList
      } else {
        //diagonal

        val xSteps = to.x - from.x
        val ySteps = to.y - from.y

        if (xSteps.abs != ySteps.abs)
          sys.error(
            s"number of steps on x and y axis need to be the same for diagonal movement: " +
              s"xSteps: $xSteps, ySteps: $ySteps"
          )

        val xStepMultiplier = if (xSteps > 0) 1 else -1
        val yStepMultiplier = if (ySteps > 0) 1 else -1

        0.to(xSteps.abs).toList.map { step =>
          Point(
            from.x + step * xStepMultiplier,
            from.y + step * yStepMultiplier
          )
        }
      }
    }

    def generateDiagram(fromToPoints: List[(Point, Point)]): Diagram = {
      val map = fromToPoints
        .flatMap(fromTo => generateLinePoints(fromTo._1, fromTo._2))
        .foldLeft(Map[Point, Int]()) { case (map, point) =>
          map.updatedWith(point) { old => old.map(_ + 1).orElse(Some(1)) }
        }
      Diagram(map)
    }
  }

}
