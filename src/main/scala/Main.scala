import scala.io.Source
import org.apache.commons.lang3.StringUtils

object Main extends App {
  type Unit = Vector[String]
  type UnitList = Vector[Unit]

  val digits = "123456789"
  val rows = "ABCDEFGHI"

  def cross(a: String, b: String) = {
    for { x <- a; y <- b } yield s"$x$y"
  }.toVector

  val cols = digits
  val squares = cross(rows, cols)
  val colUnits: UnitList = cols.map(c => cross(rows, c.toString)).toVector
  val rowUnits: UnitList = rows.map(r => cross(r.toString, cols)).toVector
  val boxUnits = for {
    x <- Set("ABC", "DEF", "GHI"); y <- Set("123", "456", "789")
  } yield cross(x, y)

  val unitList = (rowUnits ++ colUnits ++ boxUnits)
  val units = squares.foldLeft(Map[String, UnitList]()) {
    case (u, s) => u + (s -> unitList.filter(_.contains(s)))
  }
  val peers = squares.foldLeft(Map[String, Set[String]]()) {
    case (m, s) => m + (s -> (units.getOrElse(s, Nil).flatten.toSet - s))
  }

  /** cells grouped into 3 = rows
      rows grouped into 3 = matrix
      f"${str}10s"
      mkGrouped(Iterable, count, sep)
    */
  def display(gridMap: Map[String, String]) = {
    val width = squares.map(s => gridMap.getOrElse(s, "").size).max + 1
    val line = Seq(width, width, width) map { case w => "-" * w * 3 } mkString ("+", "+", "+\n")
    val body = rows map { r =>
      cols map { c =>
        gridMap.getOrElse(s"$r$c", Nil)
      } grouped (3) map { x =>
        x map (y => ("%" + width + "s").format(y)) mkString ""
      } mkString ("|", "|", "|")
    } grouped (3) map { x =>
      x.mkString("\n")
    } mkString s"\n$line"
    print(line)
    println(body)
    print(line)
  }

  def solveFile(fileName: String, jobName: String, sep: String) = {
    val filePath = getClass.getResource(fileName)
    val puzzleStrings = Source
      .fromURL(filePath)
      .getLines
      .mkString
      .replaceAll(sep, "")
      .sliding(81, 81)
      .toArray
    val (results, times) = solveAll(puzzleStrings)
    val N = puzzleStrings.size
    println(
      s"Solved ${results.sum} of $N $jobName puzzles (avg ${times.sum / N} msecs (${N * 1000 / times.sum} Hz),  max ${times.max} msecs)"
    )
  }

  def solveAll(grids: Array[String]) = {
    def solveGridTimed(grid: String): (Int, Int) = {
      val start = System.nanoTime()
      val solvedGrid = solve(grid)
      val time = (System.nanoTime() - start) / 1000000

      def checkSolution(gridMap: Map[String, String]): Boolean = {
        def isUnitSolved(unit: Unit): Boolean = {
          unit.map { s =>
            gridMap.getOrElse(s, "")(0)
          }.toSet == digits.toSet
        }
        unitList.forall(unit => isUnitSolved(unit))
      }

      val result = if (checkSolution(solvedGrid)) 1 else 0
      (result, time.asInstanceOf[Int])
    }

    val (results, times) = grids.foldLeft((Seq[Int](), Seq[Int]())) {
      case (a, g) => {
        val (result, time) = solveGridTimed(g)
        (a._1 :+ 1, a._2 :+ time)
      }
    }
    (results, times)
  }

  def solve(puzzleStr: String, show: Boolean = false) = {

    val puzzleGridMap: Map[String, String] = squares.zipWithIndex.map {
      case (s, i) =>
        (s, puzzleStr(i) match {
          case '0' => digits
          case '.' => digits
          case d   => d.toString
        })
    }.toMap

    def simpleSolve(gridMap: Map[String, String]): Map[String, String] = {

      def presentInPeers(
          values: Map[String, String],
          square: String,
          digit: Char
      ): Boolean = {
        val peerUnits = units.getOrElse(square, Nil)
        peerUnits.exists { unit =>
          unit.forall { s =>
            {
              s == square || !values.getOrElse(s, "").contains(digit)
            }
          }
        }
      }

      def remove_from_peers(
          values: Map[String, String],
          square: String,
          digit: String
      ): Map[String, String] = {
        values ++ peers.getOrElse(square, Nil).map { peer =>
          {
            val peer_values = values.getOrElse(peer, "")
            (peer, peer_values.replace(digit, ""))
          }
        }
      }

      def eliminate(
          values: Map[String, String],
          square: String,
          digit: Char
      ): Map[String, String] = {
        val cell_value = values.getOrElse(square, "")
        // remove from peers
        if (cell_value.size == 1) {
          remove_from_peers(values, square, cell_value)
        } else {
          val notFoundInPeers = cell_value.filter { digit =>
            presentInPeers(values, square, digit)
          }
          if (notFoundInPeers.size == 1) {
            remove_from_peers(values, square, notFoundInPeers) ++ Map(
              square -> notFoundInPeers
            )
          } else values
        }
      }
      val solvedMap = gridMap.foldLeft(gridMap) {
        case (a, s) => {
          val square = s._1
          val square_values = a.getOrElse(square, "")
          square_values.foldLeft(a) {
            case (a1, digit) => {
              eliminate(a1, square, digit)
            }
          }
        }
      }
      if (solvedMap == gridMap) gridMap
      else simpleSolve(solvedMap)
    }

    def dfsSolve(gridMap: Map[String, String]): Map[String, String] = {
      def zeroCondition(m: Map[String, String]): Boolean =
        m forall (_._2.size == 1)

      val solvedMap = simpleSolve(gridMap)

      if (zeroCondition(solvedMap)) solvedMap
      else {
        val (square, value) = solvedMap.filter(_._2.size != 1).minBy(_._2.size)
        value.foldLeft(solvedMap) {
          case (f, v) => {
            val newMap = dfsSolve(f ++ Map(square -> v.toString))
            if (zeroCondition(newMap)) newMap
            else f
          }
        }
      }
    }

    val solution = dfsSolve(puzzleGridMap)
    if (show) display(solution)
    solution
  }

  solveFile("p096_sudoku.txt", "easy", "Grid \\d{2}")
  solveFile("top95.txt", "hard", "")

}
