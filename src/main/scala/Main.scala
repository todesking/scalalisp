package com.todesking.scalalisp

object Extensions {
  implicit class Opt[A](self:A) {
    def toSome = Some(self)
  }
}
import Extensions._

object Main {
  implicit class SProcess(self:S) {
    def ===(expected:S):Unit = assert(self == expected, s"expected ${expected} but ${self}")
  }
  def l(values:Any*) = U.parse(values)
  def main(args:Array[String]):Unit = {
    val set_! = Sym("set!")

    def eval(ss:S*):S = {
      val env = Env.newGlobal()
      ss.foldLeft[S](SNil){(_, s) => env.eval(s)}
    }

    eval(S(1))                  === S(1)
    eval(l('+, 1, 2))             === S(3)
    eval(l('if, 1, 2, 3))         === S(2)
    eval(l('if, SNil, 2, 3))      === S(3)
    eval(l('define, 'x, 1), S('x)) === S(1)
    eval(l('define, 'x, 1), l(set_!, 'x, S(2)), S('x)) === S(2)
    eval(l(
      l('lambda, l('x),
        l('+, 'x, 1)),
      100)) === S(101)

    eval(l('define, 'plus,
        l('lambda, l('x, 'y),
          l('+, 'x, 'y)) ),
      l('define, 'foo, 100),
      l('plus, 'foo, 10)) === S(110)

    eval(l(
      l('lambda, l('x, 'y),
        l(l('lambda, l('x), l('+, 'x, 'y)),
          l('+, 'x, 1), 'y)),
      10, 20)) === S(31)

    eval(
      l('define, 'incr,
        l(
          l('lambda, l('counter),
            l('lambda, l(), l('begin,
              l(set_!, 'counter, l('+, 'counter, 1)),
              'counter))),
          0)),
      l('incr),
      l('incr)) === S(2)
    eval(
      l('define, 'x, 1),
      l(
        l('lambda, l('x), l(set_!, 'x, 10)),
        1),
      S('x)
    ) === S(1)
  }
}

abstract class Env(_values:Map[String, S]) {
  var values:Map[String, S] = _values

  def isDefined0(key:String):Boolean = values.contains(key)
  def isDefined(key:String):Boolean

  def lookup0(key:String):Option[S] = values.get(key)
  def lookup(key:String):S

  def set(key:String, value:S):Unit

  def define(key:String, value:S):Unit = values = values + (key -> value)

  def defineAll(names:Seq[String], values:Seq[S]):Unit =
    names.zip(values).foreach{case (k, v) => define(k, v)}

  def extend(names:Seq[String], values:Seq[S]) = new Env.Child(this,
    names.zip(values).foldLeft(Map.empty[String, S]) { (vs, nv) => val (n, v) = nv; vs + (n -> v) }
  )

  protected def variableNotFoundError(key:String) = throw new E(s"Variable not found: ${key}")

  override def toString = values.toString

  def eval(expr:S):S = {
    println(s"EVAL ${expr}, ENV=${this}")
    expr match {
      case Sym(name) => lookup(name)
      case n @ Num(_) => n
      case SNil => SNil
      case ProperList(func, args @ _*) =>
        evalSpecialO(func, args) getOrElse evalApply(func, args)
      case _ => throw new E(s"Unsupported expr: ${expr}")
    }
  }

  def evalSpecialO(id:S, args:Seq[S]):Option[S] = (id, args) match {
    case (Sym("if"), Seq(condS, thenS, elseS)) =>
      val cond = eval(condS)
      val body = if(cond.isTruthy) thenS else elseS
      eval(body).toSome
    case (Sym("set!"), Seq(Sym(name), valueS)) =>
      set(name, eval(valueS))
      Some(SNil)
    case (Sym("define"), Seq(Sym(name), valueS)) =>
      define(name, eval(valueS))
      Some(SNil)
    case (Sym("lambda"), Seq(ProperList(args @ _*), body:S)) =>
      Lambda(this, args.map{case Sym(name) => name case _ => throw new E("argument should symbol")}, body).toSome
    case (Sym("begin"), body) =>
      body.foldLeft[S](SNil){(_, s) => eval(s)}.toSome
    case _ => None
  }

  def evalApply(func:S, args:Seq[S]):S = {
    val evaledArgs = args.map(eval(_))
    eval(func) match {
      case NativeProc(body) => body(evaledArgs)
      case Lambda(closed, params, body) =>
        closed.extend(params, evaledArgs).eval(body)
      case unk => throw new RuntimeException(s"cant apply to ${unk}")
    }
  }
}


object Env {
  class Root(_values:Map[String, S]) extends Env(_values) {
    override def isDefined(key:String):Boolean = isDefined0(key)
    override def lookup(key:String):S = lookup0(key).getOrElse(variableNotFoundError(key))
    override def set(key:String, value:S):Unit =
      if(isDefined0(key)) define(key, value)
      else variableNotFoundError(key)
  }

  class Child(parent:Env, _values:Map[String, S]) extends Env(_values) {
    override def isDefined(key:String):Boolean = isDefined0(key) || parent.isDefined(key)
    override def lookup(key:String) = lookup0(key) getOrElse parent.lookup(key)
    override def set(key:String, value:S):Unit =
      if(isDefined0(key)) define(key, value)
      else parent.set(key, value)
  }

  def root(values:Map[String, S]) = new Root(values)

  def newGlobal():Env = {
    root(Map(
      "+" -> NativeProc { case Seq(a, b) => Num(a.as[Num].value + b.as[Num].value) }
    ))
  }
}

class E(val message:String) extends RuntimeException(message)

object U {
  def list(values:S*):S = values.foldRight[S](SNil) { (x, a) => Cons(x, a) }
  def parse(value:Any):S = value match {
    case s:S              => s
    case Seq(values @ _*) => list(values.map(U.parse):_*)
    case s:Symbol         => Sym(s.name)
    case i:Int            => Num(i)
    case _                => throw new E("Parse error")
  }
}

abstract sealed class S {
  def as[A <: S] = this.asInstanceOf[A]
  def isTruthy:Boolean = true
}

object S {
  def apply(s:Symbol) = Sym(s.name)
  def apply(n:Int) = Num(n)
}

case class Cons(car:S, cdr:S)                            extends S
case object SNil                                         extends S {
  override def isTruthy = false
}
case class Num(value:Int)                                extends S
case class Sym(value:String)                             extends S
case class NativeProc(body:Seq[S] => S)                  extends S
case class Lambda(val env:Env, args:Seq[String], body:S) extends S {
  override def toString() = s"(lambda (${args.mkString(" ")}) ${body}"
}

object ProperList {
  def unapplySeq(value:S):Option[Seq[S]] = value match {
    case Cons(a, b) => for { tail <- unapplySeq(b) } yield { a +: tail }
    case SNil       => Some(Seq())
    case _          => None
  }
}
