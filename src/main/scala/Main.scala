package com.todesking.scalalisp

object Main {
  def main(args:Array[String]):Unit = {
    val env = Env.newGlobal()

    assert(env.eval(Num(1))._1 == Num(1))
    assert(env.eval(U.list(Sym("+"), Num(1), Num(2)))._1 == Num(3))
    assert(env.eval(U.list(Sym("if"), Num(1), Num(2), Num(3)))._1 == Num(2))
    assert(env.eval(U.list(Sym("if"), SNil, Num(2), Num(3)))._1 == Num(3))
    assert(env.eval(U.list(Sym("set!"), Sym("x"), Num(1)))._2.eval(Sym("x"))._1 == Num(1))
  }
}

class Env(values:Map[String, S]) {
  def this(values:(String, S)*) = this(Map(values:_*))
  def eval(expr:S):(S, Env) = {
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
    case _ => None
  }
  def evalApply(func:S, args:Seq[S]):(S, Env) = eval(func) match {
    case (NativeProc(body), e) => (body(args), e)
    case unk => throw new RuntimeException(s"cant apply to ${unk}")
  }

  def update(name:String, value:S):Env = new Env(values + (name -> value))
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
}

abstract sealed class S {
  def as[A <: S] = this.asInstanceOf[A]
  def isTruthy:Boolean = true
}

case class Cons(car:S, cdr:S)           extends S
case object SNil                       extends S {
  override def isTruthy = false
}
case class Num(value:Int)               extends S
case class Sym(value:String)            extends S
case class NativeProc(body:Seq[S] => S) extends S

object ProperList {
  def unapplySeq(value:S):Option[Seq[S]] = value match {
    case Cons(a, b) => for { tail <- unapplySeq(b) } yield { a +: tail }
    case SNil => Some(Seq())
    case _ => None
  }
}
