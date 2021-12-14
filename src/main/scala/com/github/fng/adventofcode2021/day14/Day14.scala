package com.github.fng.adventofcode2021.day14

object Day14 {

  case class Polymer(polymer: String) {
    def process(insertionRules: InsertionRules): Polymer = {
      val resultingPolymer = polymer.foldLeft("") {
        case (resultingPolymer, char) =>
          if (resultingPolymer.isEmpty) {
            char.toString
          } else {
            val a = resultingPolymer.last.toString
            val b = char.toString
            val insertion = insertionRules.insertionForPair(s"$a$b")
            resultingPolymer + s"$insertion$b"
          }
      }
      Polymer(resultingPolymer)
    }

    def processNTimes(
        insertionRules: InsertionRules,
        numberOfTimes: Int
    ): Polymer = {
      1.to(numberOfTimes).foldLeft(this) { case (resultingPolymer, step) =>
        println(s"processing step $step")
        resultingPolymer.process(insertionRules)
      }
    }

    def countElements: List[(String, Int)] = polymer.toCharArray
      .map(_.toString)
      .groupBy(element => element)
      .map { case (key, elements) =>
        key -> elements.length
      }
      .toList

    def score: Int = {
      val max = countElements.maxBy(_._2)._2
      val min = countElements.minBy(_._2)._2
      max - min
    }

  }

  case class InsertionRules(map: Map[String, String]) {
    def insertionForPair(pair: String): String = {
      map.getOrElse(pair, sys.error(s"No rule found for $pair"))
    }
  }

  def parseInput(lines: List[String]): (Polymer, InsertionRules) = {
    val emptyLineIndex = lines.indexWhere(_.isEmpty)
    val polymer = lines
      .take(emptyLineIndex)
      .collectFirst(Polymer(_))
      .getOrElse(sys.error("No Polymer template found"))

    val instructions = lines.drop(emptyLineIndex + 1).map { instructionLine =>
      instructionLine.split(" -> ").toList match {
        case List(input, inserted) => input -> inserted
        case other                 => sys.error(s"Can't parse fold line: $instructionLine")
      }
    }
    (polymer, InsertionRules(instructions.toMap))
  }

}
