package model

sealed trait ChessPiece{

  def isThreathening(fieldToCheck:ChessField, fieldToPutPiece:ChessField): Boolean
  def sortingPriority: Int
}

case object King extends ChessPiece {

  override def isThreathening(fieldToCheck:ChessField, fieldToPutPiece:ChessField) =
    Math.abs(fieldToCheck.x - fieldToPutPiece.x) <= 1 && Math.abs(fieldToCheck.y - fieldToPutPiece.y) <= 1

  override def sortingPriority = 5
}

case object Queen extends ChessPiece {

  override def isThreathening(fieldToCheck:ChessField, fieldToPutPiece:ChessField) =
    fieldToCheck.x == fieldToPutPiece.x ||
    fieldToCheck.y == fieldToPutPiece.y ||
    Math.abs(fieldToCheck.x - fieldToPutPiece.x) == Math.abs(fieldToCheck.y - fieldToPutPiece.y)

  override def sortingPriority = 1
}

case object Rook extends ChessPiece {

  override def isThreathening(fieldToCheck:ChessField, fieldToPutPiece:ChessField) =
    fieldToCheck.x == fieldToPutPiece.x || fieldToPutPiece.y == fieldToPutPiece.y

  override def sortingPriority = 3
}

case object Bishop extends ChessPiece {

  override def isThreathening(fieldToCheck:ChessField, fieldToPutPiece:ChessField) =
    Math.abs(fieldToCheck.x - fieldToPutPiece.x) == Math.abs(fieldToCheck.y - fieldToPutPiece.y)

  override def sortingPriority = 2
}

case object Knight extends ChessPiece {

  override def isThreathening(fieldToCheck:ChessField, fieldToPutPiece:ChessField) =
    (fieldToCheck.x == fieldToPutPiece.x && fieldToCheck.y == fieldToPutPiece.y) ||
    (Math.abs(fieldToCheck.x - fieldToPutPiece.x) == 1 && Math.abs(fieldToCheck.y - fieldToPutPiece.y) == 2) ||
    (Math.abs(fieldToCheck.x - fieldToPutPiece.x) == 2 && Math.abs(fieldToCheck.y - fieldToPutPiece.y) == 1)


  override def sortingPriority = 4
}

