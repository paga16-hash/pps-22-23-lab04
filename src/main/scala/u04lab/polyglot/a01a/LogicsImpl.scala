package u04lab.polyglot.a01a

import Logics.*
import u04lab.polyglot.Pair

import scala.collection.mutable
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boat: Int) extends Logics :
  import u04lab.code.List.*
  val Failures = 5
  private val _rnd = Random(42)
  val boatRow: Int = _rnd.nextInt(size)
  val boatLeftCol: Int = _rnd.nextInt(size - boat + 1)
  var failures: Int = 0
  val hit: u04lab.code.List[(Int, Int)] = Nil()

  def hit(row: Int, col: Int): Result = (row, col) match
    case (r, c) if r == boatRow && c >= boatLeftCol && c < boatLeftCol + boat => hitted(r, c)
    case _ => failed()

  private def failed() =
    failures = failures + 1
    if failures == Failures then Result.LOST else Result.MISS

  private def hitted(row: Int, col: Int) =
    append(hit, Cons((row, col), Nil()))
    if length(hit) == boat then Result.WON else Result.HIT