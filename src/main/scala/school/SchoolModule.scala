package school

import util.Sequences.Sequence
import Sequence.*
import util.Optionals.Optional

import scala.annotation.tailrec

trait Teacher:
  def name: String

object Teacher:
  def apply(name: String): Teacher = TeacherImpl(name)
  private case class TeacherImpl(name: String) extends Teacher

trait Course:
  def name: String
  def category: String

object Course:
  def apply(name: String, category: String): Course = CourseImpl(name, category)
  def unapply(c: Course): Option[(String, String)] = Some(c.name, c.category)
  private case class CourseImpl(name: String, category: String) extends Course

trait School:
  def courses(): Sequence[String]
  def teachers(): Sequence[String]
  def setTeacherToCourse(teacher: Teacher, course: Course): School
  def coursesOfATeacher(teacher: Teacher): Sequence[Course]
  def hasTeacher(name: String): Boolean
  def hasCourse(name: String): Boolean

object School:
  def apply(school: Sequence[(Teacher, Course)]): School = SchoolImpl(school)
  def emptySchool: School = apply(Nil())

  private class SchoolImpl(school: Sequence[(Teacher, Course)]) extends School:
    override def teachers(): Sequence[String] = school.map((t, _) => t.name).distinct()
    override def courses(): Sequence[String] = school.map((_, c) => c.name).distinct()
    override def setTeacherToCourse(teacher: Teacher, course: Course): School =
      School(Cons((teacher, course), school))
    override def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
      school.filter((t, _) => t == teacher).map((_, c) => c)
    override def hasTeacher(name: String): Boolean = teachers().contains(name)
    override def hasCourse(name: String): Boolean = courses().contains(name)

object sameCategory {
  def unapply(courses: Sequence[Course]): Option[String] = 
    @tailrec
    def iter(courses: Sequence[Course], cat: Option[String]): Option[String] = courses match
      case Cons(h, t) if cat.isEmpty => iter(t, Some(h.category))
      case Cons(h, t) if cat.getOrElse("") == h.category => iter(t, cat)
      case Cons(h, _) => None
      case _ => cat
    iter(courses, None)
}

@main def examples(): Unit =
  val courses = Sequence(Course("Math", "Science"), Course("Information", "Science"), Course("Physics", "Science"))
  courses match
    case sameCategory(cat) => println(s"$courses have same category $cat")
    case _ => println (s"$courses have different categories")

