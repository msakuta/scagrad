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

  def demo_sin = {
    val tape = new Tape
    val x = tape.value("x", 0)
    val x2 = x * x
    val sin_x2 = x2(scala.math.sin, scala.math.cos)
    for i <- -50 until 50
    do
      val xval = i / 25.0
      tape.clear_data()
      x.set(xval)
      println(s"[$xval  , ${sin_x2.eval()}, ${sin_x2.derive(x)}],")
  }

  val tape = new Tape
  val x = tape.value("x", 0)
  val sigma = tape.value("sigma", 1)
  val in = -(x * x / (sigma * sigma))
  val exp = in(scala.math.exp, scala.math.exp)
  println("xval, y, dy/dx, dy/dx (backprop), dy/dsigma (backprop)")
  for i <- -50 until 50
  do
    val xval = i / 10.0
    tape.clear_data()
    x.set(xval)
    exp.backward()
    val unwrap = (x: Option[Double]) => x match {
      case Some(x) => x
      case None => 0
    }
    println(s"[$xval, ${exp.eval()}, ${exp.derive(x)}, ${unwrap(x.grad())}, ${unwrap(sigma.grad())}],")
}

class Tape {
  var terms: Array[TapeNode] = Array()

  def value(id: String, v: Double) = {
    val idx = terms.length
    terms = terms :+ TapeNode(Value(id, v))
    TapeTerm(idx, this)
  }

  def eval_int(term: Int): Double = {
    terms(term).data match {
      case Some(v) => return v
      case None => {}
    }
    val data = terms(term).term match {
      case Value(_, v) => v
      case Add(lhs, rhs) => eval_int(lhs) + eval_int(rhs)
      case Sub(lhs, rhs) => eval_int(lhs) - eval_int(rhs)
      case Mul(lhs, rhs) => eval_int(lhs) * eval_int(rhs)
      case Div(lhs, rhs) => eval_int(lhs) / eval_int(rhs)
      case Neg(term2) => -eval_int(term2)
      case UnaryFn(term2, f, _) => f(eval_int(term2))
    }
    terms(term).data = Some(data)
    data
  }

  def derive_int(term: Int, wrt: Int): Double = terms(term).term match {
    case Value(_, _) => if (wrt == term) 1 else 0
    case Add(lhs, rhs) => derive_int(lhs, wrt) + derive_int(rhs, wrt)
    case Sub(lhs, rhs) => derive_int(lhs, wrt) - derive_int(rhs, wrt)
    case Mul(lhs, rhs) => derive_int(lhs, wrt) * eval_int(rhs) + eval_int(lhs) * derive_int(rhs, wrt)
    case Div(lhs, rhs) => {
      val lhsv = eval_int(lhs)
      val rhsv = eval_int(rhs)
      derive_int(lhs, wrt) / rhsv - lhsv * derive_int(rhs, wrt) / pow(rhsv, 2)
    }
    case Neg(term) => -derive_int(term, wrt)
    case UnaryFn(term, _, grad) => grad(eval_int(term)) * derive_int(term, wrt)
  }

  def backward_int(term: Int): Unit = {
    val node = terms(term)
    node.grad = Some(1)
    for i <- term to 0 by -1 do
      val node = terms(i)
      node.term match {
        case Value(_, _) => {}
        case Add(lhs, rhs) => node.grad.map { grad =>
          terms(lhs).set_grad(grad)
          terms(rhs).set_grad(grad)
        }
        case Sub(lhs, rhs) => node.grad.map { grad =>
          terms(lhs).set_grad(grad)
          terms(rhs).set_grad(-grad)
        }
        case Mul(lhs, rhs) => node.grad.map { grad =>
          terms(lhs).set_grad(grad * eval_int(rhs))
          terms(rhs).set_grad(grad * eval_int(lhs))
        }
        case Div(lhs, rhs) => node.grad.map { grad =>
          val lhsv = eval_int(lhs)
          val rhsv = eval_int(rhs)
          terms(lhs).set_grad(grad / rhsv)
          terms(rhs).set_grad(-lhsv * grad / rhsv / rhsv)
        }
        case Neg(term) => node.grad.map { grad =>
          terms(term).set_grad(-grad)
        }
        case UnaryFn(term, _, g) => node.grad.map { grad =>
          terms(term).set_grad(g(eval_int(term)) * grad)
        }
      }
  }

  def clear_data() = for node <- terms do node.data = None
  def clear_grad() = for node <- terms do node.grad = None
}

case class TapeTerm(idx: Int, tape: Tape) {
  def eval(): Double = tape.eval_int(idx)
  def derive(wrt: TapeTerm): Double = tape.derive_int(idx, wrt.idx)
  def backward() = {
    tape.clear_grad()
    tape.backward_int(idx)
  }
  def set(v: Double) = tape.terms(idx).set(v)
  def grad(): Option[Double] = tape.terms(idx).grad

  def +(other: TapeTerm) = {
    val idx = tape.terms.length
    tape.terms = tape.terms :+ TapeNode(Add(this.idx, other.idx))
    TapeTerm(idx, tape)
  }

  def -(other: TapeTerm) = {
    val idx = tape.terms.length
    tape.terms = tape.terms :+ TapeNode(Div(this.idx, other.idx))
    TapeTerm(idx, tape)
  }

  def *(other: TapeTerm) = {
    val idx = tape.terms.length
    tape.terms = tape.terms :+ TapeNode(Mul(this.idx, other.idx))
    TapeTerm(idx, tape)
  }

  def /(other: TapeTerm) = {
    val idx = tape.terms.length
    tape.terms = tape.terms :+ TapeNode(Div(this.idx, other.idx))
    TapeTerm(idx, tape)
  }

  def unary_- = {
    val idx = tape.terms.length
    tape.terms = tape.terms :+ TapeNode(Neg(this.idx))
    TapeTerm(idx, tape)
  }

  def apply(f: (Double) => Double, g: (Double) => Double) = {
    val idx = tape.terms.length
    tape.terms = tape.terms :+ TapeNode(UnaryFn(this.idx, f, g))
    TapeTerm(idx, tape)
  }
}

class TapeNode(node: Term) {
  var data: Option[Double] = None
  var grad: Option[Double] = None
  def set(v: Double) = {
    data = None
    grad = None
    node.set(v)
  }
  def term = node
  def set_grad(v: Double) = {
    grad match {
      case Some(x) => grad = Some(x + v)
      case None => grad = Some(v)
    }
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
case class Neg(term: Int) extends Term
case class UnaryFn(term: Int, f: (Double) => Double, grad: (Double) => Double) extends Term
