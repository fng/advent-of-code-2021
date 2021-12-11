package com.github.fng.adventofcode2021.day11

object Day11 {

  case class Grid(map: Map[(Int, Int), Int], flashes: Int){
    def runSteps(numberOfSteps: Int): Grid = {
      1.to(numberOfSteps).foldLeft(this){
        case (gridBeforeStep, step) =>
          val mapAfterStepIncrease = gridBeforeStep.map.map{
            case (key, value) => key -> (value + 1)
          }
          processFlash(gridBeforeStep.copy(map = mapAfterStepIncrease))
      }
    }

    private def processFlash(grid: Grid): Grid ={
      val readyToFlash = grid.map.toList.collect{
        case (coordinate, energyLevel) if energyLevel > 9 => coordinate
      }

      if(readyToFlash.nonEmpty) {
        processFlash(readyToFlash.foldLeft(grid)(flashSingleOctopus))
      } else{
        grid
      }

    }

    private def flashSingleOctopus(grid: Grid, coordinates: (Int, Int)): Grid = {
      //set the flashing octopus to energy level 0
      val gridAfterSingleFlash = grid.copy(map = grid.map.updated(coordinates, 0), flashes = grid.flashes + 1)
      adjacentPoints(coordinates).foldLeft(gridAfterSingleFlash){
        case (gridBeforeNeighbourIncrease, point) =>
          gridBeforeNeighbourIncrease.copy(map =           gridBeforeNeighbourIncrease.map.updatedWith(point){
            case Some(0) => Some(0) //0 means an already flashed Octopus which should not be increased further
            case Some(before) => Some(before + 1)
            case None => None
          }
          )
      }

    }

    private def adjacentPoints(point: (Int, Int)): List[(Int, Int)] = {
      (-1).to(1).flatMap{xDelta =>
        (-1).to(1).map{yDelta =>
          (point._1 + xDelta, point._2 + yDelta)
        }
      }.toList.filterNot(_ == point)
    }

    override def toString: String = {
      val maxX = map.keys.toList.map(_._1).max
      val maxY = map.keys.toList.map(_._1).max

      0.to(maxY)
        .map { y =>
          0.to(maxX)
            .map { x =>
              map((x, y))
            }
            .mkString
        }
        .mkString("\n")

    }
  }

  def parseGrid(input: List[String]): Grid = {
    Grid(
      input.zipWithIndex.flatMap { case (line, y) =>
        line.toCharArray.toList.map(_.toString.toInt).zipWithIndex.map {
          case (energyLevel, x) =>
            (x, y) -> energyLevel
        }
      }.toMap,
      flashes = 0
    )
  }

}
