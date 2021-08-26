package br.unb.cic.wlang

import org.scalatest.funsuite.AnyFunSuite

class CFGBuilderBuilderTest extends AnyFunSuite {

  test("simple-CFG") {
    val stmt = Assignment("x", Const(4), 1)
    val program = WhileProgram(stmt)

    val g = CFGBuilder.flow(program)

    val expected = Set()

    assert(expected == g)
  }

  test("factorial-CFG") {
    val d1 = Assignment("y", Var("x"), 1)
    val d2 = Assignment("z", Const(1), 2)
    val d3 = Assignment("z", Mult(Var("z"), Var("y")), 4)
    val d4 = Assignment("y", Sub(Var("y"), Const(1)), 5)
    val w1 = While(Condition(GT(Var("y"), Const(1)), 3), Sequence(d3, d4))
    val d5 = Assignment("y", Const(0), 6)

    val p = WhileProgram(Sequence(d1, Sequence(d2, Sequence(w1, d5))))

    val g = CFGBuilder.flow(p)

    val expected: Set[(Int, Int)] =
      Set((1, 2)
         ,(2, 3)
         ,(3, 4)
         ,(4, 5)
         ,(5, 3)
         ,(3, 6))

    assert(expected == g)
  }

  test("power-CFG") {
    val d1 = Assignment("z", Const(1), 1)
    val d2 = Assignment("z", Mult(Var("z"), Var("y")), 3)
    val d3 = Assignment("x", Sub(Var("x"), Const(1)), 4)
    val w1 = While(Condition(GT(Var("x"), Const(0)), 2), Sequence(d2, d3))

    val p = WhileProgram(Sequence(d1,w1))

    val g = CFGBuilder.flow(p)

    val expected: Set[(Int, Int)] =
      Set((1, 2)
         ,(2, 3)
         ,(3, 4)
         ,(4, 2))

    assert(expected == g)
  }

  test("factorial-CFG-inverse") {
    val d1 = Assignment("y", Var("x"), 1)
    val d2 = Assignment("z", Const(1), 2)
    val d3 = Assignment("z", Mult(Var("z"), Var("y")), 4)
    val d4 = Assignment("y", Sub(Var("y"), Const(1)), 5)
    val w1 = While(Condition(GT(Var("y"), Const(1)), 3), Sequence(d3, d4))
    val d5 = Assignment("y", Const(0), 6)

    val p = WhileProgram(Sequence(d1, Sequence(d2, Sequence(w1, d5))))

    val g = CFGBuilder.flowInverse(p)

    val expected: Set[(Int, Int)] =
      Set((2, 1)
         ,(3, 2)
         ,(4, 3)
         ,(5, 4)
         ,(3, 5)
         ,(6, 3))

    assert(expected == g)
  }

  test("power-CFG-inverse") {
    
    val d1 = Assignment("z", Const(1), 1)
    val d2 = Assignment("z", Mult(Var("z"), Var("y")), 3)
    val d3 = Assignment("x", Sub(Var("x"), Const(1)), 4)
    val w1 = While(Condition(GT(Var("x"), Const(0)), 2), Sequence(d2, d3))

    val p = WhileProgram(Sequence(d1,w1))

    val g = CFGBuilder.flowInverse(p)

    val expected: Set[(Int, Int)] =
      Set((2, 1)
         ,(3, 2)
         ,(4, 3)
         ,(2, 4))

    assert(expected == g)
  }

}
