package com.github.fng.adventofcode2021.day04

object Day04 {

  case class DrawnNumbers(numbers: List[Int]) {
    def draw(number: Int) = this.copy(numbers :+ number)
  }

  case class BoardNumber(number: Int, isDrawn: Boolean) {
    override def toString: String = {
      val numberString = if (number < 10) s" $number" else s"$number"
      if (isDrawn) s"<$numberString>" else numberString
    }

    def draw(number: Int): BoardNumber = {
      this.copy(isDrawn = isDrawn || this.number == number)
    }
  }
  case class Board(matrix: List[List[BoardNumber]], hasBingo: Boolean) {
    override def toString: String = {
      matrix
        .map { line =>
          line.mkString(" ")
        }
        .mkString("\n")
    }

    def draw(number: Int): Board = {
      val newMatrix = matrix.map(_.map(_.draw(number)))

      val bingoInHorizontalLine = newMatrix.exists(_.forall(_.isDrawn))
      val bingoInVerticalLine = newMatrix.transpose.exists(_.forall(_.isDrawn))
      Board(newMatrix, hasBingo = bingoInHorizontalLine || bingoInVerticalLine)
    }

    def calculateScore(lastDrawnNumber: Int): Option[Int] = {
      Option.when(hasBingo) {
        matrix.flatten.filterNot(_.isDrawn).map(_.number).sum * lastDrawnNumber
      }
    }

  }

  def parseInput(input: List[String]): (DrawnNumbers, List[Board]) = {

    val drawnNumbers = DrawnNumbers(input.head.split(",").map(_.toInt).toList)

    val boards = input.tail
      .filterNot(_ == "") //remove empty lines
      .grouped(5) //group 5 lines together
      .map { boardLines =>
        val matrix = boardLines.map { boardLine =>
          boardLine
            .replaceAll("  ", " ")
            .stripPrefix(" ")
            .split(" ")
            .map(_.toInt)
            .toList
            .map(BoardNumber(_, isDrawn = false))
        }
        Board(matrix, hasBingo = false)
      }
      .toList

    (drawnNumbers, boards)

  }

}
