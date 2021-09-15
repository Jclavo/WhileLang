package br.unb.cic.wlang.parser

import br.unb.cic.wlang._
import org.scalatest.funsuite.AnyFunSuite

class WhileProgramParserTest extends AnyFunSuite {
  val p = new WhileProgramParser()

  test("Test for the skip parser") {
    p.parse(p.skip, "skip") match {
      case p.Success(Skip(1), _) => succeed
      case p.Failure(msg,_) => println(s"FAILURE: $msg")
      case p.Error(msg,_) => println(s"ERROR: $msg")
    }
  }

  test("Test for the const parser") {
    p.parse(p.const, "123") match {
      case p.Success(Const(123), _) => succeed
      case p.Failure(msg,_) => println(s"FAILURE: $msg")
      case p.Error(msg,_) => println(s"ERROR: $msg")
    }
  }

  test("Test for the assignment parser") {
    p.parse(p.assignment, "x := 123") match {
      case p.Success(Assignment("x", Const(123), 1), _) => succeed
      case p.Failure(msg,_) => println(s"FAILURE: $msg")
      case p.Error(msg,_) => println(s"ERROR: $msg")
    }
  }

  test("Test for a simple arithmetic expression") {
    p.parse(p.aExp, "123 + 456 * 3") match {
      case p.Success(exp, _) => assert(exp == Add(Const(123), Mult(Const(456), Const(3))))
      case p.Failure(msg,_) => println(s"FAILURE: $msg")
      case p.Error(msg,_) => println(s"ERROR: $msg")
    }
  }

  test("Test for arithmetic expressions with brackets") {
    p.parse(p.aExp, "(123 + 456) / 3") match {
      case p.Success(exp, _) => assert(exp == Div(Add(Const(123), Const(456)), Const(3)))
      case p.Failure(msg,_) => println(s"FAILURE: $msg")
      case p.Error(msg,_) => println(s"ERROR: $msg")
    }
  }

  test("Test for a simple LT expression") {
    p.parse(p.rel, "x < 0") match {
      case p.Success(exp, _) => assert(exp == LT(Var("x"), Const(0)))
      case p.Failure(msg,_) => println(s"FAILURE: $msg"); fail()
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail()
    }
  }

  test("Test for non trivial boolean expressions") {
    p.parse(p.bExp, "x < 0 || (y > x + 1 && y < 100)") match {
      case p.Success(exp, _) => assert(exp == Or(LT(Var("x"), Const(0)), And(GT(Var("y"), Add(Var("x"), Const(1))), LT(Var("y"), Const(100)))))
      case p.Failure(msg,_) => println(s"FAILURE: $msg"); fail
      case p.Error(msg,_) => println(s"ERROR: $msg"); fail
    }
  }
}
