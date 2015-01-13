/**
 * Inspired by Peter Norvig's essay on sudoku solvers.
 * Implemented using only immutable state.
 * Conducts depth-first search with a minimum remaining values heuristic
 * while quickly ruling out values using constraint propagation to fail-fast,
 * as described in the essay.
 *
 * http://norvig.com/sudoku.html
 */

import scala.annotation.tailrec
import scala.collection.immutable.Seq

object Sudoku {

  type Values = Seq[Int]
  type Board = Seq[Values]
  type Solution = Seq[Int]

  def eliminate(board: Board, r: Int, c: Int, d: Int): Option[Board] = {
    val i = index(r, c)
    if (!board(i).contains(d)) {
      Some(board)
    }
    else {
      val eliminated = board.updated(
        i,
        board(i).filterNot(_ == d)
      )

      if (eliminated(i).isEmpty) {
        None
      }
      else if (eliminated(i).size == 1) {
        val candidateVal = eliminated(i).head

        @tailrec
        def propagate(board: Board, peers: Seq[Int]): Option[Board] = {
          if (peers.isEmpty) {
            Some(board)
          }
          else {
            val nextPeer = peers.head
            eliminate(
              board,
              row(nextPeer),
              col(nextPeer),
              candidateVal
            ) match {
              case Some(x) =>
                propagate(x, peers.tail)
              case None =>
                None
            }
          }
        }

        propagate(eliminated, peers(r, c))
      }
      else {
        Some(eliminated)
      }
    }
  }

  def assign(board: Board, r: Int, c: Int, d: Int): Option[Board] = {
    val i = index(r, c)
    val otherVals = board(i).filterNot(_ == d)

    @tailrec
    def eliminateAll(board: Board, otherVals: Seq[Int]): Option[Board] = {
      if (otherVals.isEmpty) {
        Some(board)
      }
      else {
        val nextVal = otherVals.head
        eliminate(
          board,
          r,
          c,
          nextVal
        ) match {
          case Some(x) =>
            eliminateAll(x, otherVals.tail)
          case None =>
            None
        }
      }
    }

    eliminateAll(board, otherVals)
  }

  def solve(board: Board): Option[Solution] = {

    @tailrec
    def search(queue: Seq[Board]): Option[Board] = {
      if (queue.isEmpty) {
        None
      }
      else {
        val board = queue.head
        if (board.forall(_.size == 1)) {
          Some(board)
        }
        else {
          val nextIndex = board.indexOf(board.filter(_.size > 1).minBy(_.size))
          val nextBoards = (1 to 9).map { d =>
            assign(board, row(nextIndex), col(nextIndex), d)
          }.flatten
          search(nextBoards ++ queue.tail)
        }
      }
    }

    val solutionBoard = search(Seq(board))
    solutionBoard.map { board =>
      board.flatten
    }
  }

  def printGrid(solution: Solution) {
    val rows = solution.zipWithIndex.groupBy(_._2 / 9)
    for (i <- (0 until 9)) {
      val row = rows(i).map { lol =>
        lol._1
      }.mkString(" ")
      println(row)
    }
  }

  def index(r: Int, c: Int) = (r * 9 + c)
  def row(i: Int) = i / 9
  def col(i: Int) = i % 9

  def peers(r: Int, c: Int): Seq[Int] = {
    val i = index(r, c)
    (0 until 81)
      .filterNot(_ == i)
      .filter { n =>
      row(n) == r ||
        col(n) == c ||
        (row(n) / 3 == r / 3) && (col(n) / 3 == c / 3)
    }
  }

}
