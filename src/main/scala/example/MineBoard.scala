package example

import scala.util.Random

class MineBoard(val width: Int, val height: Int) {
  val PercentageOfMines = 75
  val board: Array[Array[MineCell]] = {
    val random = Random
    Range(0, height).toArray.map(_ =>
      Range(0, width).toArray.map(_ =>
        MineCell(random.nextInt(100) > PercentageOfMines)
      )
    )
  }

  private def getCell(x: Int, y: Int) = board(y)(x)

  def flagCell(selectedX: Int, selectedY: Int): Boolean = {
    val selectedCell = getCell(selectedX, selectedY)
    selectedCell.toggleFlag()
    true // always continue game after flagging a cell
  }

  // "cascade" in this case is cell being opened because it's adjacent to the user's selection
  def openCell(selectedX: Int, selectedY: Int, selectedViaCascase: Boolean = false): Boolean = {
    val selectedCell = getCell(selectedX, selectedY)
    selectedCell.isOpen || { // short-circuit if cell is already open
      val adjacentCellCoords = getAdjacentCellCoords(selectedX, selectedY)
      val adjacentMineCount = adjacentCellCoords.count(coord => getCell(coord._1, coord._2).mine)
      val itWasntAMine = selectedCell.open(adjacentMineCount)
      if(itWasntAMine) {
        // only cascade to adjacent cells for the user's selection
        // or when the cascaded cell has no adjacent mines
        if(!selectedViaCascase || adjacentMineCount == 0) {
          val adjacentNonMineCoords = adjacentCellCoords
            .filter(coord => !getCell(coord._1, coord._2).mine)
          adjacentNonMineCoords.foreach(coord =>
            openCell(coord._1, coord._2, selectedViaCascase = true))
        }
      }
      itWasntAMine
    }
  }

  private def getAdjacentCellCoords(selectedX: Int, selectedY: Int): List[(Int, Int)] = {
    val increments = -1 to 1
    val incrementTuples = increments.flatMap(x => increments.map(y => (x, y)))
    incrementTuples.flatMap({
      case(incrementX, incrementY) =>
        val newX = selectedX + incrementX
        val newY = selectedY + incrementY
        if(
          (incrementX == 0 && incrementY == 0) || // skip the current cell
            newX < 0 ||
            newX >= width ||
            newY < 0 ||
            newY >= height // and other cells not on the board
        ) {
          None
        } else {
          Some(newX, newY)
        }
    }).toList
  }

  override def toString: String = {
    // top labels
    Range('a', 'a' + width).map(_.toChar).mkString("   ", "   ", "\n") +
    board.zipWithIndex.map({
      case(row: Array[MineCell], index:Int) =>
        // left labels
        index + 1 + " " +
          // cells themselves
          row.mkString(" ")
    }).mkString("\n")
  }
}
