package example

object Status extends Enumeration {
  type Status = Value
  val Untouched, Flagged, Opened = Value
}
