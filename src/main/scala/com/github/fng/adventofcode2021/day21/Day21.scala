package com.github.fng.adventofcode2021.day21

object Day21 {

  trait Dice {
    def numberOfRolls: Int
    def roll(): Int
  }

  class DeterministicDice extends Dice {
    private var _nextNumber = 1;

    def numberOfRolls: Int = _nextNumber - 1

    def roll(): Int = {
      val rolledNumber = _nextNumber
      _nextNumber = _nextNumber + 1
      rolledNumber
    }

  }

  case class Player(
      name: String,
      startPosition: Int,
      position: Int,
      score: Int
  ) {
    def move(rolls: Int): Player = {
      val newPosition = ((position + rolls) % 10) match {
        case 0     => 10
        case other => other
      }
      val newScore = score + newPosition

      println(
        s"Player $name rolls $rolls and moves to space $newPosition for a total score of $newScore"
      )

      copy(position = newPosition, score = newScore)
    }
  }

  case class Board(
      player1: Player,
      player2: Player,
      dice: Dice,
      gameScore: Option[Long] = None
  ) {

    def playTillWin(): Board = {
      val newPlayer1 = player1.move(dice.roll() + dice.roll() + dice.roll())
      if (newPlayer1.score >= 1000) {
        val gameScore = player2.score * dice.numberOfRolls
        copy(player1 = newPlayer1, gameScore = Some(gameScore))
      } else {
        val newPlayer2 = player2.move(dice.roll() + dice.roll() + dice.roll())
        val newBoard = copy(player1 = newPlayer1, player2 = newPlayer2)
        if (newPlayer2.score >= 1000) {
          val gameScore = newPlayer1.score * dice.numberOfRolls
          newBoard.copy(gameScore = Some(gameScore))
        } else {
          newBoard.playTillWin()
        }
      }
    }

  }

  object Board {
    def newBoard(
        player1StartPosition: Int,
        player2StartPosition: Int,
        dice: Dice
    ): Board =
      apply(
        Player("1", player1StartPosition, player1StartPosition, 0),
        Player("2", player2StartPosition, player2StartPosition, 0),
        dice
      )
  }

}
