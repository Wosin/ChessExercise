import model._

import scala.annotation.tailrec

object Boot extends App {

  override def main(args: Array[String]): Unit = {
    val t0 = System.nanoTime();
    val result = putAllPiecesOnFields(Seq(King, King, Queen, Queen, Bishop, Bishop, Knight).sortBy(_.sortingPriority), Set(ChessBoard(7,7)))
    val t1 = System.nanoTime();
    println("TIME TOOK:" + (t1-t0)/1000000000.0)
    println("RESULTS", result.size)
  }

  @tailrec
  def putAllPiecesOnFields(piecesLeftToPut: Seq[ChessPiece], precalculatedBoards: Set[ChessBoard]): Set[ChessBoard] = {
    piecesLeftToPut match {
      case Nil => precalculatedBoards
      case piece :: tail => {
        val boardsForCurrentPiece = precalculatedBoards.flatMap(b => calculateAllPositionsForPiece(b,piece))
        putAllPiecesOnFields(tail, boardsForCurrentPiece)
      }
    }
  }

  def calculateAllPositionsForPiece(chessBoard: ChessBoard, piece: ChessPiece): Set[ChessBoard] = for {
      field <- chessBoard.freeFields
      precalculatedBoards <- findFieldForPiece(chessBoard.freeFields, chessBoard.piecesOnFields, field, piece)
    } yield precalculatedBoards


  def findFieldForPiece(emptyFields: Set[ChessField], piecesOnFields: Map[ChessField, ChessPiece], field: ChessField, pieceToPut: ChessPiece): Option[ChessBoard] = {
    if(!piecesOnFields.keySet.exists(fieldTaken => isFieldInThreat(pieceToPut, fieldTaken, field))) {
      val pieceOnField = field -> pieceToPut
      Some(ChessBoard(emptyFields.filter(fieldToCheck => !isFieldInThreat(pieceToPut, fieldToCheck, field)), piecesOnFields + pieceOnField))
    } else {
      None
    }

  }

  def isFieldInThreat(chessPiece: ChessPiece, fieldTaken: ChessField, fieldToCheck:ChessField) : Boolean = {
    chessPiece.isThreathening(fieldTaken, fieldToCheck)
  }
}
