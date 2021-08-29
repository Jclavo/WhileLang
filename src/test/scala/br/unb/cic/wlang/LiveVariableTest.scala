package br.unb.cic.wlang
import org.scalatest.funsuite.AnyFunSuite

class LiveVariableTest extends AnyFunSuite {
 

  // val s3 = Assignment("y", Sub(Var("a"), Var("b")), 3)
  // val s4 = Assignment("y", Sub(Var("b"), Var("a")), 4)
  // val s5 = Assignment("x", Sub(Var("a"), Var("b")), 5)
  // val s1 = IfThenElse(Condition(GT(Var("a"),Var("b")),1),Sequence(s2,s3),Sequence(s4,s5))

  val s1 = Assignment("x", Const(2), 1)
  val s2 = Assignment("y", Const(4), 2)
  val s3 = Assignment("x", Const(1), 3) 
  val s5 = Assignment("z", Var("y"), 5)
  val s6 = Assignment("z", Mult(Var("y"), Var("y")), 6)
  val s7 = Assignment("x", Var("z"), 7)

  val s4 = IfThenElse(Condition(GT(Var("y"),Var("x")), 4), s5, s6)
  
  val p = WhileProgram(Sequence(s1,Sequence(s2,Sequence(s3,Sequence(s4,s7)))))

  test("my-test-1") {
    val (in, out) = LiveVariable.execute(p)

    assert(in(1) ==  Set.empty)
    assert(out(1) ==  Set.empty)

    assert(in(2) == Set.empty)
    assert(out(2) == Set(Var("y")))

    assert(in(3) == Set(Var("y")))
    assert(out(3) ==  Set(Var("x"),Var("y")))
  
    assert(in(4) == Set(Var("x"),Var("y")))
    assert(out(4) == Set(Var("y")))

    assert(in(5) ==  Set(Var("y")))
    assert(out(5) == Set(Var("z")))

    assert(in(6) ==  Set(Var("y")))
    assert(out(6) == Set(Var("z")))

    assert(in(7) == Set(Var("z")))
    assert(out(7) == Set.empty)

    // assert(in(2) == Set.empty)
    // assert(out(2) == Set("y"))

    // assert(in(3) == Set("y"))
    // assert(out(3) ==  Set("x","y"))
  
    // assert(in(4) == Set("x","y"))
    // assert(out(4) == Set("y"))

    // assert(in(5) ==  Set("y"))
    // assert(out(5) == Set("z"))

    // assert(in(6) ==  Set("y"))
    // assert(out(6) == Set("z"))

    // assert(in(7) == Set("z"))
    // assert(out(7) == Set.empty)

    
  }

}
