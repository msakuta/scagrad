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
    def derive_sin(in: Int, out: Int, der: Int): Option[Int] = Some(tape.add_unary(in, scala.math.sin, scala.math.cos, derive_sin))
    val sin_x2 = x2(scala.math.sin, scala.math.cos, derive_sin)
    for i <- -50 until 50
    do
      val xval = i / 10.0
      tape.clear_data()
      x.set(xval)
      println(s"[$xval, ${sin_x2.eval()}, ${sin_x2.derive(x)}],")
  }

  def derive_exp(tape: Tape, in: Int, out: Int, der: Int): Option[Int] = Some(tape.add_mul(out, der))

  def demo_exp = {
    val tape = new Tape
    val x = tape.value("x", 0)
    val sigma = tape.value("sigma", 1)
    val in = -(x * x / (sigma * sigma))
    val exp = in(scala.math.exp, scala.math.exp, { (in, out, der) => derive_exp(tape, in, out, der) })
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

  def demo_higher_order: Unit = {
    val tape = new Tape
    val x = tape.value("x", 0)
    val sigma = tape.value("sigma", 1)
    val in = -(x * x / (sigma * sigma))
    val exp = in(scala.math.exp, scala.math.exp, { (in, out, der) => derive_exp(tape, in, out, der) })
    val exp2 = exp.gen_graph(x) match {
      case Some(x) => x
      case None => return
    }
    val exp3 = exp2.gen_graph(x) match {
      case Some(x) => x
      case None => return
    }
    println("xval, y, dy/dx, dy/dx (backprop), dy/dsigma (backprop)")
    for i <- -50 until 50
    do
      val xval = i / 10.0
      tape.clear_data()
      x.set(xval)
      val unwrap = (x: Option[Double]) => x match {
        case Some(x) => x
        case None => 0
      }
      println(s"[$xval, ${exp.eval()}, ${exp.derive(x)}, ${exp2.eval()}, ${exp3.eval()}],")
  }

  demo_sin
}

class Tape {
  var terms: Array[TapeNode] = Array(TapeNode(Value("0", 0)), TapeNode(Value("1", 1)))

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
      case UnaryFn(term2, f, _, _) => f(eval_int(term2))
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
    case UnaryFn(term, _, grad, _) => grad(eval_int(term)) * derive_int(term, wrt)
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
        case UnaryFn(term, _, g, _) => node.grad.map { grad =>
          terms(term).set_grad(g(eval_int(term)) * grad)
        }
      }
  }

  def gen_graph(idx: Int, wrt: Int): Option[Int] = {
    val ret = terms(idx).term match {
      case Value(_, _) => if (idx == wrt) { Some(1) } else { None }
      case Add(lhs, rhs) => {
        (gen_graph(lhs, wrt), gen_graph(rhs, wrt)) match {
          case (Some(lhs), None) => Some(lhs)
          case (None, Some(rhs)) => Some(rhs)
          case (Some(lhs), Some(rhs)) => Some(add_add(lhs, rhs))
          case _ => None
        }
      }
      case Sub(lhs, rhs) => {
        (gen_graph(lhs, wrt), gen_graph(rhs, wrt)) match {
          case (Some(lhs), None) => Some(lhs)
          case (None, Some(rhs)) => Some(add_neg(rhs))
          case (Some(lhs), Some(rhs)) => Some(add_sub(lhs, rhs))
          case _ => None
        }
      }
      case Mul(lhs, rhs) => {
        (gen_graph(lhs, wrt), gen_graph(rhs, wrt)) match {
          case (Some(dlhs), None) => Some(add_mul(dlhs, rhs))
          case (None, Some(drhs)) => Some(add_mul(lhs, drhs))
          case (Some(dlhs), Some(drhs)) => {
            val plhs = add_mul(dlhs, rhs)
            val prhs = add_mul(lhs, drhs)
            val node = add_add(plhs, prhs)
            Some(node)
          }
          case _ => None,
        }
      }
      case Div(lhs, rhs) => {
        (gen_graph(lhs, wrt), gen_graph(rhs, wrt)) match {
          case (Some(dlhs), None) => Some(add_div(dlhs, rhs))
          case (None, Some(drhs)) => {
            Some(add_neg(add_div(add_div(add_mul(lhs, drhs), rhs), rhs)))
          }
          case (Some(dlhs), Some(drhs)) => {
            val plhs = add_div(dlhs, rhs)
            val prhs = add_div(add_div(add_mul(lhs, drhs), rhs), rhs)
            Some(add_sub(plhs, prhs))
          }
          case _ => None,
        }
      }
      case Neg(term) => gen_graph(term, wrt).map({ node => add_neg(node) })
      case UnaryFn(term, _, _, gg) => {
        val derived = gen_graph(term, wrt)
        derived.flatMap({ derived => gg(term, idx, derived)})
      }
    }
    ret
  }

  def add_add(lhs: Int, rhs: Int) = {
    val id = terms.length
    terms = terms :+ TapeNode(Add(lhs, rhs))
    id
  }

  def add_sub(lhs: Int, rhs: Int) = {
    val id = terms.length
    terms = terms :+ TapeNode(Sub(lhs, rhs))
    id
  }

  def add_mul(lhs: Int, rhs: Int) = {
    val id = terms.length
    terms = terms :+ TapeNode(Mul(lhs, rhs))
    id
  }

  def add_div(lhs: Int, rhs: Int) = {
    val id = terms.length
    terms = terms :+ TapeNode(Div(lhs, rhs))
    id
  }

  def add_neg(term: Int) = {
    val id = terms.length
    terms = terms:+ TapeNode(Neg(term))
    id
  }

  def add_unary(term: Int, f: (Double) => Double, g: (Double) => Double, gg: (Int, Int, Int) => Option[Int]) = {
    val id = terms.length
    terms = terms :+ TapeNode(UnaryFn(term, f, g, gg))
    id
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
  def gen_graph(wrt: TapeTerm): Option[TapeTerm] = tape.gen_graph(idx, wrt.idx).map({ x => TapeTerm(x, tape) })
  def set(v: Double) = tape.terms(idx).set(v)
  def grad(): Option[Double] = tape.terms(idx).grad
  def +(other: TapeTerm) = TapeTerm(tape.add_add(idx, other.idx), tape)
  def -(other: TapeTerm) = TapeTerm(tape.add_sub(idx, other.idx), tape)
  def *(other: TapeTerm) = TapeTerm(tape.add_mul(idx, other.idx), tape)
  def /(other: TapeTerm) = TapeTerm(tape.add_div(idx, other.idx), tape)
  def unary_- = TapeTerm(tape.add_neg(idx), tape)

  def apply(f: (Double) => Double, g: (Double) => Double, gg: (Int, Int, Int) => Option[Int]) = {
    TapeTerm(tape.add_unary(idx, f, g, gg), tape)
  }
}

class TapeNode(value: Term) {
  var data: Option[Double] = None
  var grad: Option[Double] = None
  def set(v: Double) = {
    data = None
    grad = None
    value.set(v)
  }
  def term = value
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
case class UnaryFn(term: Int, f: (Double) => Double, grad: (Double) => Double, gen_graph: (Int, Int, Int) => Option[Int]) extends Term
