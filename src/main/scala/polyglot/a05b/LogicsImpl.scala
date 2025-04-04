package polyglot.a05b

import util.Sequences.Sequence
import Sequence.*

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:
  private val initial = (Random.nextInt(size), Random.nextInt(size))
  private var ticked: Sequence[(Int, Int)] = Cons(initial, Nil())
  private var addendum = 0
  override def tick(): Unit =
    addendum = addendum + 1
    for
      x <- -1 to 1
      y <- -1 to 1
      if x != 0 || y != 0
    do ticked = Cons((initial._1 + x * addendum, initial._2 + y * addendum), ticked)

  override def isOver: Boolean = ticked.filter((x, y) => x < 0 || x >= size || y < 0 || y >= size) != Nil()

  override def hasElement(x: Int, y: Int): Boolean = ticked.contains((x, y))
