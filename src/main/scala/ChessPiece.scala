sealed trait ChessPiece{
  def x: Int
  def y: Int
  def priority: Int
}

case class King(x: Int, y: Int) extends ChessPiece {
  override def priority: Int = 1
}

case class Queen(x: Int, y: Int) extends ChessPiece {
  override def priority: Int = 5
}

case class Rook(x: Int, y: Int) extends ChessPiece {
  override def priority: Int = 4
}

case class  Bishop(x: Int, y: Int) extends ChessPiece {
  override def priority: Int = 3
}

case class Knight(x: Int, y: Int) extends ChessPiece {
  override def priority: Int = 2
}