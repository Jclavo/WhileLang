package br.unb.cic.wlang.df.framework

import br.unb.cic.wlang.WhileProgram
import br.unb.cic.wlang.WhileProgram.{Label, finalLabels, initLabel}
import br.unb.cic.wlang.cfg.CFGBuilder
import br.unb.cic.wlang.cfg.CFGBuilder.CFG

import scala.collection.mutable
import scala.collection.mutable.HashMap

trait AnalysisDirection

case object ForwardAnalysis extends AnalysisDirection
case object BackwardAnalysis extends AnalysisDirection

abstract class MFP[Abstraction](wp: WhileProgram) {

  type MFPResult = mutable.Map[Label, Set[Abstraction]]

  def execute(): (MFPResult, MFPResult) = {
    // Step1: Initialization (of w and analysis)
    val f = buildControlFlowGraph()
    val extremeLabels = findExtremeLabels()

    var w : List[(Label, Label)] = List()
    val analysis : MFPResult = new mutable.HashMap()

    for((l1, l2) <- f) {
      w = (l1, l2) :: w      // "cons" in Scala is '::'
    }

    val allLabels: Set[Label] =  f.map( { case (l1, l2) => Set(l1, l2) }).fold(Set[Label]())(_ union _)

    for(l <- allLabels union extremeLabels) {
      if(extremeLabels.contains(l)) analysis(l) = extremeValues()
      else analysis(l) = lattice().bottom
    }

    // Step2: Iteration (updating w and analysis)
    while(!w.isEmpty) {
      val (l1, l2) = w.head
      w = w.tail
      if(! lattice().orderOperator(transferFunction(analysis(l1), l1), analysis(l2))) {
        analysis(l2) = lattice().meetOperator(analysis(l2), transferFunction(analysis(l1), l1))
        for( (a, b) <- f.filter( { case (a, _) => a == l2 }) ) {
          w = (a, b) :: w
        }
      }
    }

    val mfp1: MFPResult = analysis
    val mfp2: MFPResult = new mutable.HashMap()

    for(l <- allLabels) {
      mfp2(l) = transferFunction(analysis(l), l)
    }

    (mfp1, mfp2)
  }

  def transferFunction(analysis: Set[Abstraction], label: Label): Set[Abstraction] =
    (analysis diff kill(label)) union gen(label)

  def buildControlFlowGraph(): CFG = direction() match {
    case ForwardAnalysis => CFGBuilder.flow(wp)
    case BackwardAnalysis => CFGBuilder.flowR(wp)
  }

  def findExtremeLabels() : Set[Label] = direction() match {
    case ForwardAnalysis => Set(initLabel(wp))
    case BackwardAnalysis => finalLabels(wp)
  }

  /* these abstract definitions correspond to the 'hot spots' of our framework */
  def kill(label: Label): Set[Abstraction]
  def gen(label: Label): Set[Abstraction]

  def lattice(): Lattice[Abstraction]
  def direction(): AnalysisDirection
  def extremeValues(): Set[Abstraction]
}
