package com.github.fng.adventofcode2021.day17

object Day17 {

  case class Point(x: Int, y: Int)

  case class TargetArea(from: Point, to: Point) {
    val targetMap: Set[Point] = {
      val minX = from.x
      val maxX = to.x
      val minY = to.y
      val maxY = from.y

      Set(
        minY
          .to(maxY, -1)
          .flatMap { y =>
            minX
              .to(maxX)
              .map { x =>
                Point(x, y)
              }
          }: _*
      )
    }

    def contains(point: Point): Boolean = targetMap.contains(point)

  }

  case class Velocity(x: Int, y: Int)

  case class Probe(position: Point, velocity: Velocity, path: List[Point]) {
    def calculateNextStep(): Probe = {
      val newPosition = position.copy(
        x = position.x + velocity.x,
        y = position.y + velocity.y
      )

      val newVelocity = velocity.copy(
        x = math.max(0, if (velocity.x > 0) velocity.x - 1 else velocity.x + 1),
        y = velocity.y - 1
      )
      Probe(newPosition, newVelocity, path :+ newPosition)
    }
  }

  object TargetComputer {
    def initialize(
        targetArea: TargetArea,
        initialVelocity: Velocity
    ): TargetComputer =
      TargetComputer(
        targetArea,
        initialVelocity,
        Probe(Point(0, 0), initialVelocity, Nil),
        0
      )

    def trajectoryWithHighestY(targetArea: TargetArea): TargetComputer = {
      val velocities = 0
        .to(100)
        .flatMap { x =>
          (-100).to(100).map { y =>
            Velocity(x, y)
          }
        }
        .toList

      velocities
        .map(initialize(targetArea: TargetArea, _).calculateTrajectory())
        .filter(_.isProbeInTargetArea)
        .maxBy(_.probe.path.map(_.y).max)
    }

  }

  case class TargetComputer(
      targetArea: TargetArea,
      initialVelocity: Velocity,
      probe: Probe,
      steps: Int
  ) {
    def calculateNextStep(): TargetComputer = {
      this.copy(
        probe = this.probe.calculateNextStep(),
        steps = this.steps + 1
      )
    }

    def calculateTrajectory(): TargetComputer = {
      calculateTrajectoryRec(this)
    }

    def calculateTrajectoryRec(
        targetComputer: TargetComputer
    ): TargetComputer = {
      val next = targetComputer.calculateNextStep()
      if (next.isProbeInTargetArea) next
      else if (next.isProbeBeyondTargetArea) next
      else calculateTrajectoryRec(next)
    }

    lazy val isProbeInTargetArea: Boolean = targetArea.contains(probe.position)

    lazy val isProbeBeyondTargetArea: Boolean = {
      val targetMinY = targetArea.targetMap.map(_.y).min
      probe.position.y < targetMinY
    }

    val maxProbeHeight: Int =
      if (probe.path.isEmpty) 0 else probe.path.map(_.y).max

    override def toString: String = {
      val targetMaxX = targetArea.to.x
      //y is negative
      val targetMaxY = targetArea.from.y

      val probeMinY =
        if (probe.path.isEmpty) 0 else math.max(0, probe.path.map(_.y).max)
      val probeMaxY = if (probe.path.isEmpty) 0 else probe.path.map(_.y).min

      val probeMaxX = if (probe.path.isEmpty) 0 else probe.path.map(_.x).max

      val minY = probeMinY
      val maxY = math.min(targetMaxY, probeMaxY)
      val maxX = math.max(targetMaxX, probeMaxX)

      val map = minY
        .to(maxY, -1)
        .map { y =>
          0.to(maxX)
            .map { x =>
              val point = Point(x, y)
              if (point == Point(0, 0)) "S"
              else if (point == probe.position) "P"
              else if (probe.path.contains(point)) "#"
              else if (targetArea.contains(point)) "T"
              else "."
            }
            .mkString
        }
        .mkString("\n")

      val probeInTarget =
        if (isProbeInTargetArea) " <- PROBE IN TARGET AREA!!!!" else ""

      s"""Initial velocity: ${initialVelocity.x}, ${initialVelocity.y}
         |After Step: $steps$probeInTarget
         |max height: $maxProbeHeight
         |$map
         |""".stripMargin
    }

  }

  def parseInput(input: String): TargetArea = {
    input.stripPrefix("target area: ").split(", ").toList match {
      case List(x, y) =>
        val (fromX, toX) = x.stripPrefix("x=").split("\\.\\.").toList match {
          case List(fromX, toX) => fromX.toInt -> toX.toInt
        }
        val (fromY, toY) = y.stripPrefix("y=").split("\\.\\.").toList match {
          case List(fromY, toY) => fromY.toInt -> toY.toInt
        }
        TargetArea(Point(fromX, fromY), Point(toX, toY))
    }
  }

}
