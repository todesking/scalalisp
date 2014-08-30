package com.todesking.scalalisp

object Main {
  implicit class ToS(value:Any) {
    def toS:S = U.parse(value)
  }
  implicit class SProcess(ve:(S, Env)) {
    def >>(expr:S):(S, Env) = ve._2.eval(expr)
    def ===(expected:S):Unit = assert(ve._1 == expected, s"expected ${expected} but ${ve._1}")
  }
  def l(values:Any*) = U.parse(values)
  def main(args:Array[String]):Unit = {
    val set_! = Sym("set!")

    val env = Env.newGlobal()

    env.eval(Num(1)) === Num(1)
    env.eval(l('+, 1, 2)) === Num(3)
    env.eval(l('if, 1, 2, 3)) === Num(2)
    env.eval(l('if, SNil, 2, 3)) === Num(3)
    env.eval(l(set_!, 'x, 1)) >> 'x.toS === Num(1)
    env.eval(l(
      l('lambda, l('x),
        l('+, 'x, 1)),
      100)) === Num(101)

    env.eval(l(set_!, 'plus,
        l('lambda, l('x, 'y),
          l('+, 'x, 'y)) )) >>
      l(set_!, 'foo, 100) >>
      l('plus, 'foo, 10) === Num(110)

    env.eval(l(
      l('lambda, l('x, 'y),
        l(l('lambda, l('x), l('+, 'x, 'y)),
          l('+, 'x, 1), 'y)),
      10, 20)) === Num(31)

    env.eval(
      l(set_!, 'incr,
        l(
          l('lambda, l('counter),
            l('lambda, l(), l('begin,
              l(set_!, 'counter, l('+, 'counter, 1)),
              'counter))),
          0))) >>
      l('incr) >>
      l('incr) === Num(2)
  }
}

class Env(values:Map[String, S]) {
  def this(values:(String, S)*) = this(Map(values:_*))
  override def toString = values.toString
  def eval(expr:S):(S, Env) = {
    println(s"EVAL ${expr}, ENV=${this}")
    expr match {
      case Sym(name) => (values.getOrElse(name, throw new E(s"Symbol not found: ${name}")), this)
      case Num(n) => (Num(n), this)
      case ProperList(func, args @ _*) =>
        evalSpecialO(func, args) getOrElse evalApply(func, args)
      case SNil => (SNil, this)
      case _ => throw new E(s"Unsupported expr: ${expr}")
    }
  }

  def evalSpecialO(id:S, args:Seq[S]):Option[(S, Env)] = (id, args) match {
    case (Sym("if"), Seq(condS, thenS, elseS)) =>
      val (cond, e) = eval(condS)
      if(cond.isTruthy) Some(e.eval(thenS))
      else              Some(e.eval(elseS))
    case (Sym("set!"), Seq(Sym(name), valueS)) =>
      val (value, e) = eval(valueS)
      Some((SNil, e.update(name, value)))
    case (Sym("lambda"), Seq(ProperList(args @ _*), body:S)) =>
      Some(
        Lambda(this, args.map{case Sym(name) => name case _ => throw new E("argument should symbol")}, body),
        this
      )
    case (Sym("begin"), body) => Some(
      body.foldLeft[(S, Env)]((SNil, this)) { (re, expr) => val (_, env) = re; env.eval(expr) }
    )
    case _ => None
  }
  def evalApply(func:S, args:Seq[S]):(S, Env) = {
    val (evaledArgs, env) = evalSeq(args)
    env.eval(func) match {
      case (NativeProc(body), e) => (body(evaledArgs), e)
      case (lambda @ Lambda(closed, params, body), e) =>
        closed.extend(params, evaledArgs).eval(body) match {
          case (result, closed) => lambda.env = closed; (result, e)
        }
      case unk => throw new RuntimeException(s"cant apply to ${unk}")
    }
  }

  def evalSeq(exprs:Seq[S]):(Seq[S], Env) = exprs.foldLeft((Seq[S](), this)) { (re, expr) => val (results, env) = re; val (result, newe) = env.eval(expr); (results :+ result, newe) }

  def update(name:String, value:S):Env = new Env(values + (name -> value))

  def extend(names:Seq[String], values:Seq[S]) = new Env(
    names.zip(values).foldLeft(this.values) { (vs, nv) => val (n, v) = nv; vs + (n -> v) }
  )
}

object Env {
  def newGlobal():Env = {
    new Env(
      "+" -> NativeProc { case Seq(a, b) => Num(a.as[Num].value + b.as[Num].value) }
    )
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

case class Cons(car:S, cdr:S)                            extends S
case object SNil                                         extends S {
  override def isTruthy = false
}
case class Num(value:Int)                                extends S
case class Sym(value:String)                             extends S
case class NativeProc(body:Seq[S] => S)                  extends S
case class Lambda(var env:Env, args:Seq[String], body:S) extends S

object ProperList {
  def unapplySeq(value:S):Option[Seq[S]] = value match {
    case Cons(a, b) => for { tail <- unapplySeq(b) } yield { a +: tail }
    case SNil       => Some(Seq())
    case _          => None
  }
}
