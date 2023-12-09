package example

import scala.math.pow

object Hello extends App {
  val tape = new Tape
  val a = tape.value("a", 123)
  val b = tape.value("b", 123)
  val c = tape.value("c", 42)
  val ab = a + b
  val abc = ab * c
  println(abc.eval)
  println(abc.derive(a))

  val x = tape.value("x", 0)
  val sin_x = x.apply(scala.math.sin, scala.math.cos)
  println(sin_x.eval)
  println(sin_x.derive(x))
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
  def eval(): Double = {
    tape.eval_int(idx)
  }

  def derive(wrt: TapeTerm): Double = {
    tape.derive_int(idx, wrt.idx)
  }

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

sealed trait Term
case class Value(id: String, a: Double) extends Term
case class Add(lhs: Int, rhs: Int) extends Term
case class Sub(lhs: Int, rhs: Int) extends Term
case class Mul(lhs: Int, rhs: Int) extends Term
case class Div(lhs: Int, rhs: Int) extends Term
case class UnaryFn(term: Int, f: (Double) => Double, grad: (Double) => Double) extends Term
