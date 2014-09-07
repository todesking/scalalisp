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
    eval(l(set_!, 'x, 1), S('x)) === S(1)
    eval(l(
      l('lambda, l('x),
        l('+, 'x, 1)),
      100)) === S(101)

    eval(l(set_!, 'plus,
        l('lambda, l('x, 'y),
          l('+, 'x, 'y)) ),
      l(set_!, 'foo, 100),
      l('plus, 'foo, 10)) === S(110)

    eval(l(
      l('lambda, l('x, 'y),
        l(l('lambda, l('x), l('+, 'x, 'y)),
          l('+, 'x, 1), 'y)),
      10, 20)) === S(31)

    eval(
      l(set_!, 'incr,
        l(
          l('lambda, l('counter),
            l('lambda, l(), l('begin,
              l(set_!, 'counter, l('+, 'counter, 1)),
              'counter))),
          0)),
      l('incr),
      l('incr)) === S(2)
  }
}

class Env(var values:Map[String, S]) {

  def lookup(key:String):S =
    values.getOrElse(key, throw new E(s"Symbol not found: ${key}"))

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
      update(name, eval(valueS))
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
        closed.updateAll(params, evaledArgs)
        closed.eval(body)
      case unk => throw new RuntimeException(s"cant apply to ${unk}")
    }
  }

  def update(name:String, value:S):Unit =
    this.values = values + (name -> value)
  def updateAll(names:Seq[String], values:Seq[S]):Unit =
    names.zip(values).foreach{case (k, v) => update(k, v)}

  def extend(names:Seq[String], values:Seq[S]) = new Env(
    names.zip(values).foldLeft(this.values) { (vs, nv) => val (n, v) = nv; vs + (n -> v) }
  )
}

object Env {
  def newGlobal():Env = {
    new Env(Map(
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
