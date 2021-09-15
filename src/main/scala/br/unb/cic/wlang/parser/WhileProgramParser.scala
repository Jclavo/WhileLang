package br.unb.cic.wlang.parser

import br.unb.cic.wlang._

import scala.util.parsing.combinator._

class WhileProgramParser extends JavaTokenParsers  {

  var cl = 0    // the current label

  def wp: Parser[WhileProgram] = "begin" ~ statement ~ "end" ^^ { case _ ~ _ =>  WhileProgram(List(), Skip(1)) }

  /* building blocks for parsing statements */
  def statement: Parser[Stmt] = skip

  def skip: Parser[Stmt] = "skip" ^^ { case _ => {cl = cl + 1; Skip(cl) } }

  def assignment: Parser[Stmt] = ident ~ ":=" ~ aExp ^^ { case v ~ _ ~ exp => cl = cl + 1; Assignment(v, exp, cl)}


  /*
   * building blocks for parsing arithmetic expressions
   * this is the idiomatic way of dealing with
   * operator "priority" using the standard
   * Scala parser combinator library.
   *
   * although idiomatic, it is not so easy
   * to understand.
   */

  def aExp  : Parser[AExp] = term ~ rep(plus | minus) ^^ {case a~b => (a /: b)((acc,f) => f(acc))}
  def plus  : Parser[AExp => AExp] = "+" ~ term ^^ { case "+" ~ b => Add(_, b) }
  def minus : Parser[AExp => AExp] = "-" ~ term ^^ { case "-" ~ b => Sub(_, b) }
  def term  : Parser[AExp] = factor ~ rep(times | divide) ^^ { case a~b => (a /: b)((acc,f) => f(acc))}
  def times : Parser[AExp => AExp] =  "*" ~ factor ^^ { case "*" ~ b => Mult(_, b) }
  def divide : Parser[AExp => AExp] =  "/" ~ factor ^^ { case "/" ~ b => Div(_, b) }
  def factor: Parser[AExp] = variable | const | "(" ~> aExp <~ ")"
  def variable: Parser[AExp] = ident ^^ { case name => Var(name) }
  def const : Parser[AExp] = decimalNumber ^^ { case d => Const(d.toInt) }

  /*
   * building blocks for parsing boolean expressions.
   */
  def bExp: Parser[BExp] = bTerm ~ rep(or) ^^ { case a~b => (a /: b)((acc,f) => f(acc)) }
  def or: Parser[BExp => BExp] = "||" ~ bTerm ^^ { case "||" ~ b => Or(_, b)}
  def bTerm: Parser[BExp] = (bFactor | rel) ~ rep(and) ^^ { case a~b => (a /: b)((acc,f) => f(acc)) }
  def and: Parser[BExp => BExp] = "&&" ~ (bFactor | rel) ^^ { case "&&" ~ b => And(_, b)}
  def rel: Parser[BExp] = eq | neq | gt | lt
  def eq: Parser[BExp] = aExp ~ "==" ~ aExp  ^^ { case left ~ _ ~ right => Eq(left, right) }
  def neq: Parser[BExp] = aExp ~ "!=" ~ aExp ^^ { case left ~ _ ~ right => NEq(left, right) }
  def gt: Parser[BExp] = aExp ~ ">" ~ aExp ^^ { case left ~ _ ~ right => GT(left, right) }
  def lt: Parser[BExp] = aExp ~ "<" ~ aExp ^^ { case left ~ _ ~ right => LT(left, right) }
  def bFactor: Parser[BExp] = trueExp | falseExp | "!" ~> bExp | "(" ~> bExp <~ ")"
  def trueExp: Parser[BExp] = "true" ^^ { case "true" => True}
  def falseExp: Parser[BExp] = "false" ^^ { case "false" => True}
}
