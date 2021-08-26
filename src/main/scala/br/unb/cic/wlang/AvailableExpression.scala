package br.unb.cic.wlang

import scala.collection.mutable
import CFGBuilder.flow
import WhileProgram.{Label, labels, block, initLabel, fv, assignments, nonTrivialExpression, expHasVariable}

/**
 * Implementation of the Reaching Definition
 * algorithm.
 */
object AvailableExpression {

  type Abstraction = Set[(Exp)]
  type DS = mutable.HashMap[Int, Abstraction]

  var bottom: Abstraction = Set.empty

  val undef = -1   // this is the equivalent to the undef label in the book (?)

  val entry: mutable.HashMap[Int, Abstraction] = mutable.HashMap()
  val exit: mutable.HashMap[Int, Abstraction] = mutable.HashMap()

  def execute(program: WhileProgram): (DS, DS) = {
    var fixed = false
    bottom = nonTrivialExpression(program)

    // writing entry and exits as functions would be a possible
    // solution. nonetheless, since one depends on each other,
    // and the CFG might include cycles, a table-based implementation
    // avoids infinite loops.
    val titlesTable = Seq("label","Entry","Kill","Gen","Exit")

    // we need to initialize exit..., since we have
    // to first compute entry[l] from exit[l]. after
    // that, we recompute exit[l] from entry[l].
    for(label <- labels(program)) {
      exit(label) = bottom
    }

    var iteration = 1
    do {
      var table = Seq(titlesTable)
      //println(s"\n>> iteration: ${iteration}")
      
      val entryOld = entry.clone()
      val exitOld = exit.clone()
      for(label <- labels(program)) {
        entry(label) =
          if (label == initLabel(program.stmt))
            Set.empty
          else {
            // U { exit(from) | (from, to) <- flow(program) and to == label}
            // we could have implemented this using foldl, though I hope this
            // solution here is easier to understand.
            var res = bottom
            for((from, to) <- flow(program) if to == label) {
              // if(res == bottom) res = exit(from)
              // else if (exit(from) != bottom && res != bottom) res = exit(from) intersect res
              res = exit(from) intersect res
            }
            res
          }
        val b = block(label, program)  // block with a given label *label*
        val kills = kill(b.get, program)
        val gens = gen(b.get)
        exit(label) = (entry(label) diff kills) union gens

        table = table :+ Seq(label.toString,entry(label).mkString(" "),kills.mkString(" "),gens.mkString(" "),exit(label).mkString(" "))
      }
      //println(UtilFormatTable.run(table))
      iteration += 1 
      fixed = (entryOld, exitOld) == (entry, exit)
    }
    while(! fixed)
    (entry, exit)
  }

  /* kill definition according to Table 2.1 of the ppl book */
  def kill(block: Block, program: WhileProgram): Set[Exp] = block match {
    case Assignment(v, exp, _) =>  findExpUsingVar(v, nonTrivialExpression(exp)) union nonTrivialExpression(exp).filter(e =>  expHasVariable(v,exp))
    case Skip(_) => Set.empty
    case Condition(_, _) => Set.empty
  }

  /* gen definition according to Table 2.1 of the PPL book */
  def gen(block: Block): Set[Exp] = block match {
    case Assignment(v, exp, _) => nonTrivialExpression(exp).filterNot(e =>  expHasVariable(v,exp))
    case Skip(_) => Set.empty
    case Condition(exp, _) => nonTrivialExpression(exp)
  }

  /* search for exp that use v(var) in HashMap "Exit" */
  def findExpUsingVar(v: String, exp: Set[Exp]): Set[Exp] =  {  

    var used : Set[Exp] = Set.empty

    for((key, value) <- exit){
      used = used union (for { exp <- value; if (expHasVariable(v,exp)) } yield { exp })
    }
    used     
  }
}
