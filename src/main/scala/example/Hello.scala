package example

import scala.math.pow

object Hello extends App {
  def demo = {
    val tape = new Tape
    val a = tape.value("a", 123)
    val b = tape.value("b", 123)
    val c = tape.value("c", 42)
    val ab = a + b
    val abc = ab * c
    println(abc.eval())
    println(abc.derive(a))
  }

  val tape = new Tape
  val x = tape.value("x", 0)
  val x2 = x * x
  val sin_x2 = x2(scala.math.sin, scala.math.cos)
  for i <- -50 until 50
  do
    val xval = i / 10.0
    x.set(xval)
    println(s"[$i, ${sin_x2.eval()}, ${sin_x2.derive(x)}],")
}

class Tape {
  var terms: Array[Term] = Array()

  def value(id: String, v: Double) = {
    val idx = terms.length
    terms = terms :+ (Value(id, v))
    TapeTerm(idx, this)
  }

  def eval_int(term: Int): Double = terms(term) match {
    case Value(_, v) => v
    case Add(lhs, rhs) => eval_int(lhs) + eval_int(rhs)
    case Sub(lhs, rhs) => eval_int(lhs) - eval_int(rhs)
    case Mul(lhs, rhs) => eval_int(lhs) * eval_int(rhs)
    case Div(lhs, rhs) => eval_int(lhs) / eval_int(rhs)
    case UnaryFn(term, f, _) => f(eval_int(term))
  }

  def derive_int(term: Int, wrt: Int): Double = terms(term) match {
    case Value(_, _) => if (wrt == term) 1 else 0
    case Add(lhs, rhs) => derive_int(lhs, wrt) + derive_int(rhs, wrt)
    case Sub(lhs, rhs) => derive_int(lhs, wrt) - derive_int(rhs, wrt)
    case Mul(lhs, rhs) => derive_int(lhs, wrt) * eval_int(rhs) + eval_int(lhs) * derive_int(rhs, wrt)
    case Div(lhs, rhs) => derive_int(lhs, wrt) / eval_int(rhs) - eval_int(lhs) * derive_int(rhs, wrt) / pow(eval_int(lhs), 2)
    case UnaryFn(term, _, grad) => grad(eval_int(term)) * derive_int(term, wrt)
  }
}

case class TapeTerm(idx: Int, tape: Tape) {
  def eval(): Double = tape.eval_int(idx)
  def derive(wrt: TapeTerm): Double = tape.derive_int(idx, wrt.idx)
  def set(v: Double) = tape.terms(idx).set(v)

  def +(other: TapeTerm) = {
    val idx = tape.terms.length
    tape.terms = tape.terms :+ Add(this.idx, other.idx)
    TapeTerm(idx, tape)
  }

  def -(other: TapeTerm) = {
    val idx = tape.terms.length
    tape.terms = tape.terms :+ Div(this.idx, other.idx)
    TapeTerm(idx, tape)
  }

  def *(other: TapeTerm) = {
    val idx = tape.terms.length
    tape.terms = tape.terms :+ Mul(this.idx, other.idx)
    TapeTerm(idx, tape)
  }

  def /(other: TapeTerm) = {
    val idx = tape.terms.length
    tape.terms = tape.terms :+ Div(this.idx, other.idx)
    TapeTerm(idx, tape)
  }

  def apply(f: (Double) => Double, g: (Double) => Double) = {
    val idx = tape.terms.length
    tape.terms = tape.terms :+ UnaryFn(this.idx, f, g)
    TapeTerm(idx, tape)
  }
}

sealed trait Term {
  def set(v: Double): Unit = throw Exception()
}
case class Value(id: String, var a: Double) extends Term {
  override def set(v: Double) = this.a = v
}
case class Add(lhs: Int, rhs: Int) extends Term
case class Sub(lhs: Int, rhs: Int) extends Term
case class Mul(lhs: Int, rhs: Int) extends Term
case class Div(lhs: Int, rhs: Int) extends Term
case class UnaryFn(term: Int, f: (Double) => Double, grad: (Double) => Double) extends Term
