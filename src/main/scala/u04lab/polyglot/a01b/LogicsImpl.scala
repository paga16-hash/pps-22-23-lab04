package u04lab.polyglot.a01b

import u04lab.code.List.{Nil, length}

import scala.jdk.javaapi.OptionConverters
import u04lab.polyglot.{OptionToOptional, Pair}
import u04lab.code.Option
import u04lab.code.Option.*

import scala.collection.mutable
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics :

  import u04lab.code.List.*

  var minesSet: u04lab.code.List[(Int, Int)] = Nil()
  val selected: u04lab.code.List[(Int, Int)] = Nil()
  private val _rnd = Random(42)
  while (length(minesSet) != mines) minesSet = append(minesSet, Cons((_rnd.nextInt(size), _rnd.nextInt(size)), Nil()))


  private def neighbours(x: Int, y: Int) =
    import u04lab.code.Stream.*
    import u04lab.code.List.filter
    length(filter(flatMap(toList(take(iterate(x - 1)(_ + 1))(3)))(xx => toList(map(take(iterate(y - 1)(_ + 1))(3))(yy => (xx, yy)))))(p => contains(minesSet, p)))

  def hit(x: Int, y: Int): java.util.Optional[Integer] = (x, y) match
    case (x, y) if contains(minesSet, (x, y)) => java.util.Optional.empty();
    case _ => append(selected, Cons((x, y), Nil())); java.util.Optional.of(neighbours(x, y))

  def won: Boolean =
    length(selected) + length(minesSet) == size * size