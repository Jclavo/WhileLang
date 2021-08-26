package br.unb.cic.wlang

import scala.collection.mutable
import CFGBuilder.flowInverse
import WhileProgram.{Label, labels, block, finalLabels, fv, assignments, nonTrivialExpression, expHasVariable}

/**
 * Implementation of the Reaching Definition
 * algorithm.
 */
object LiveVariable {

  type Abstraction = Set[(VarExp)]
  type DS = mutable.HashMap[Int, Abstraction]

  val bottom: Abstraction = Set.empty

  val undef = -1   // this is the equivalent to the undef label in the book (?)

  val entry: mutable.HashMap[Int, Abstraction] = mutable.HashMap()
  val exit: mutable.HashMap[Int, Abstraction] = mutable.HashMap()

  def execute(program: WhileProgram): (DS, DS) = {
    var fixed = false

    // writing entry and exits as functions would be a possible
    // solution. nonetheless, since one depends on each other,
    // and the CFG might include cycles, a table-based implementation
    // avoids infinite loops.
    val titlesTable = Seq("label","Entry","Kill","Gen","Exit")

    // we need to initialize exit..., since we have
    // to first compute entry[l] from exit[l]. after
    // that, we recompute exit[l] from entry[l].
    for(label <- labels(program)) {
      entry(label) = bottom
    }

    var iteration = 1
    do {
      var table = Seq(titlesTable)
      //println(s"\n>> iteration: ${iteration}")
      
      val entryOld = entry.clone()
      val exitOld = exit.clone()
      
      for(label <- labels(program)) {
        exit(label) =
          if (finalLabels(program.stmt).contains(label))
            bottom
          else {
            // U { exit(from) | (from, to) <- flowInverse(program) and to == label}
            // we could have implemented this using foldl, though I hope this
            // solution here is easier to understand.
            var res = bottom
            for((from, to) <- flowInverse(program) if to == label) {
              // if(res == bottom) res = entry(from)
              // else if (entry(from) != bottom && res != bottom) res = entry(from) intersect res
              res = entry(from) union res
            }
            res
          }

        val b = block(label, program)  // block with a given label *label*
        val kills = kill(b.get, program)
        val gens = gen(b.get)
        entry(label) = (exit(label) diff kills) union gens

        table = table :+ Seq(label.toString,entry(label).mkString(" "),kills.mkString(" "),gens.mkString(" "),exit(label).mkString(" "))
      }
      //println(UtilFormatTable.run(table))
      iteration += 1 
      fixed = (entryOld, exitOld) == (entry, exit)
    }
    while(! fixed)
    (entry, exit)
  }

  /* kill definition according to Table 2.3 of the ppl book */
  def kill(block: Block, program: WhileProgram): Set[VarExp] = block match {
    case Assignment(v, exp, _) =>  Set(Var(v))
    case Skip(_) => Set.empty
    case Condition(_, _) => Set.empty
  }

  /* gen definition according to Table 2.3 of the PPL book */
  def gen(block: Block): Set[VarExp] = block match {
    case Assignment(v, exp, _) => for { x <- fv(exp) } yield { Var(x) }
    case Skip(_) => Set.empty
    case Condition(exp, _) => for { x <- fv(exp) } yield { Var(x) }
    // fv(exp).filter(x => Var(x))
  }

}
