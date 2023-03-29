package u04lab.polyglot.a05b

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(
                  private val size: Int
                ) extends Logics :
  private val _rnd = Random()
  private val init: (Int, Int) = (_rnd.nextInt(size - 2) + 1, _rnd.nextInt(size - 2) + 1)
  private var tickCount: Int = 0

  override def tick(): Unit =
    tickCount = tickCount + 1

  override def hasElement(x: Int, y: Int): Boolean =
    import Math.abs
    (x == init._1 && abs(y - init._2) <= tickCount) ||
      (y == init._2 && abs(x - init._1) <= tickCount) ||
      (x - y == init._1 - init._2 && abs(x - init._1) <= tickCount) ||
      (x + y == init._1 + init._2 && abs(x - init._1) <= tickCount)

  override def isOver: Boolean =
    init._2 - tickCount < 0 || init._2 + tickCount >= size ||
      init._1 - tickCount < 0 || init._1 + tickCount >= size
