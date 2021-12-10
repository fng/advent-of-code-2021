package com.github.fng.adventofcode2021.day10

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object Day10 {

  sealed trait SyntaxResult
  case object Valid extends SyntaxResult
  case object Incomplete extends SyntaxResult
  case class Corrupted(unexpectedChar: String) extends SyntaxResult {
    val errorScore: Long = unexpectedChar match {
      case ")" => 3
      case "]" => 57
      case "}" => 1197
      case ">" => 25137
    }
  }

  case class CorruptedSyntaxException(corrupted: Corrupted)
      extends RuntimeException

  private val validOpenChars = List("(", "{", "<", "[")

  def validateLineOfChunks(lineOfChunks: String): SyntaxResult = {
    Try {
      lineOfChunks.toCharArray.toList.map(_.toString).foldLeft(List[String]()) {
        case (stack, char) =>
          //empty stack
          if (stack.isEmpty) {
            //char is open
            if (validOpenChars.contains(char)) {
              char :: stack
            } else {
              throw CorruptedSyntaxException(Corrupted(char))
            }
          } else {

            //open char on stack
            if (validOpenChars.contains(stack.head)) {
              //char is open
              if (validOpenChars.contains(char)) {
                char :: stack
                //closing char
              } else {
                stack.head match {
                  case "(" =>
                    if (char == ")") stack.tail
                    else throw CorruptedSyntaxException(Corrupted(char))
                  case "{" =>
                    if (char == "}") stack.tail
                    else throw CorruptedSyntaxException(Corrupted(char))
                  case "<" =>
                    if (char == ">") stack.tail
                    else throw CorruptedSyntaxException(Corrupted(char))
                  case "[" =>
                    if (char == "]") stack.tail
                    else throw CorruptedSyntaxException(Corrupted(char))
                }
              }
            } else {
              throw new RuntimeException("can we end up here????")
            }

          }
      }
    } match {
      case Failure(CorruptedSyntaxException(corrupted)) => corrupted
      case Success(stack)                               => if (stack.isEmpty) Valid else Incomplete
    }

  }

}
