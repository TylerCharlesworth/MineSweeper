package example

import example.Status._

case class MineCell(mine: Boolean = false, private var status: Status.Value = Untouched) {
  private var adjacentMineCount: Option[Int] = None

  def open(adjacentMineCount: Int): Boolean = {
    this.adjacentMineCount = Some(adjacentMineCount)
    status = Opened
    !mine // continue game if this wasn't a mine
  }

  def isOpen: Boolean = status == Opened

  def toggleFlag(): Unit = {
    status = status match {
      case Opened => Opened
      case Flagged => Untouched
      case _ => Flagged
    }
  }

  private def openedCellRepresentation = {
    val openedCount = Array(" _", " 1", " 2", " 3", " 4", " 5", " 6", " 7", " 8", "??")
    if(mine)
      "💥"
    else
      openedCount(adjacentMineCount.getOrElse(9))
  }

  override def toString: String = {
    val Debug = false
    // not wrapping in Try because we know with certainty that it's exhaustive and sealed
    (status match {
      case Untouched => "🔲"
      case Flagged => "🚩"
      case Opened => openedCellRepresentation
    })
      .concat(if(Debug && mine) "*" else " ")
  }
}
