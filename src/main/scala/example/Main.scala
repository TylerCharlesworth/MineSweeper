package example

import java.io.IOException

import scala.sys.process._
import scala.io.StdIn.readLine

// TODO: Productize this code! There are a lot of "TODO" statements throughout.
//  As this was an exercise, the tail end of development was cut for time.
//  "Perfect" is the enemy of "good enough" :-)
//
// assumptions:
// - board width and height are single-digit (else alignment and input reading will be off)
// - output font is monospaced, emoji characters take up 2 spaces

object Main extends App {
  // TODO: create MineGame class and move game logic there
  private def readMove(width: Int, height: Int): Tuple3[Int, Int, Boolean] = {
    val endChar = ('a' + width - 1).toChar
    val entryPattern = s"^([a-$endChar])([1-$height])(-?)$$".r
    LazyList.continually({
      val entry = readLine(
        s"""Please enter X coordinates (a-$endChar) followed by Y coordinates (1-$height), ex. "a1".
           |Include a trailing dash to flag a cell, ex. "a1-".
           |Your move, player: """.stripMargin)
      entryPattern.findAllMatchIn(entry).
        nextOption()
        .map(group => (
          group.group(1).charAt(0) - 'a',
          group.group(2).toInt - 1,
          group.group(3) == "-"
        ))
    }).dropWhile(_.isEmpty).head.getOrElse(throw new IOException("Error parsing console input"))
  }

  // generate board
  val mineBoard = new MineBoard(9, 9)

  // show board
  "clear".! // equivalent to print("\u001b[2J"); print("\u001b[H") for a library-less approach
  println(mineBoard)

  // continue receiving game moves until game is over
  LazyList.continually({
    // read input
    val (moveX, moveY, moveFlagged) = readMove(mineBoard.width, mineBoard.height)
    "clear".! // equivalent to print("\u001b[2J"); print("\u001b[H") for a library-less approach

    // respond to input
    val continueGame = if(moveFlagged) {
      mineBoard.flagCell(moveX, moveY)
    } else {
      mineBoard.openCell(moveX, moveY)
    }

    val headerMessage = (if (moveFlagged) "Flagged" else "Selected")
      .concat(s" x=$moveX, y=$moveY.")
      .concat(if(!moveFlagged) " May the odds be ever in your favor.\n" else "\n")
    println(headerMessage)

    // show board
    println(mineBoard)

    // TODO: detect *successful* game completion
    continueGame
  }).dropWhile(_ == true).head
  // TODO: refactor to avoid board printing duplication

  println("\nGame Over!\n")
  // TODO: detect if win/loss
}
