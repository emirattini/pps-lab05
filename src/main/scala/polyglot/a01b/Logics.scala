package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import polyglot.a01b.Logics
import util.Sequences.Sequence
import Sequence.*

import scala.annotation.tailrec
import scala.jdk.javaapi.OptionConverters
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */

trait Logics:
  def hit(x: Int, y: Int): java.util.Optional[Integer]
  def won: Boolean

class LogicsImpl(private val size: Int, private val minesNumber: Int) extends Logics:
  val mines: Sequence[(Int, Int)] = generatePoints(minesNumber, size)
  var hit: Sequence[(Int, Int)] = empty

  @tailrec
  private def generatePoints(number: Int, bound: Int, points: Sequence[(Int, Int)] = Nil()): Sequence[(Int, Int)] =
    val point = (Random.nextInt(bound), Random.nextInt(bound))
    if number == 0 then points
    else if points.contains(point) then generatePoints(number, bound, points)
    else generatePoints(number - 1, bound, Cons(point, points))

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    OptionToOptional(
      if !mines.contains((x, y))
      then
        hit = Cons((x, y), hit)
        ScalaOptional.Just(mines.filter((x1, y1) => x1 < x+2 && x1 > x-2 && y1 < y+2 && y1 > y-2).size())
      else ScalaOptional.Empty()) // Option => Optional converter

  def won = hit.filter(p => mines.contains(p)) == empty && mines.size() + hit.size() == size*size 



