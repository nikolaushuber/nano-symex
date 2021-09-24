
import java.io.{PrintWriter, OutputStreamWriter,
                BufferedReader, InputStreamReader}

import scala.util.matching.Regex

object SMT {

  class SMTException(msg : String) extends Exception(msg)

}

/**
 * Simple interface to SMT solvers.
 */
abstract class SMT {
  val name : String

  /**
   * Declare a new constant of the given type.
   */
  def declareConst(name : String, typ : String) : Unit

  /**
   * Declare a new constant of the given type and return its name.
   */
  def freshConst(typ : String) : String

  /**
   * Add a constraint to the assertion stack.
   */
  def addAssertion(str : String) : Unit

  /**
   * Add a new frame to the assertion stack.
   */
  def push : Unit

  /**
   * Pop the topmost frame from the assertion stack.
   */
  def pop : Unit

  /**
   * Check whether the current combination of constraints is satisfiable.
   */
  def isSat : Boolean

  /**
   * Query the value of a given constant.
   */
  def getSatValue(name : String) : BigInt

  /** 
   * Query array
   */
  def getArrayValue(name : String) : Map[BigInt, BigInt]

  /**
   * Reset the SMT solver to the initial state.
   */
  def reset : Unit

  /**
   * Shut down the SMT solver.
   */
  def shutdown : Unit

  /**
   * Enable or disable logging output.
   */
  def logCommands(flag : Boolean)
}

abstract class SMTProcess(cmd : Array[String]) extends SMT {
  import SMT._

  private val process = Runtime.getRuntime.exec(cmd)
  private val stdin   = process.getOutputStream
  private val stderr  = process.getErrorStream
  private val stdout  = process.getInputStream

  private val stdinWriter  = new PrintWriter (new OutputStreamWriter(stdin))
  private val stdoutReader = new BufferedReader (new InputStreamReader(stdout))

  private var nameCounter = 0
  private var logCmds     = false

  val NumberPattern: Regex = " ([0-9]+)".r.unanchored
  val NegnumPattern: Regex = "- ([0-9]+)".r.unanchored
  val SatregNegNum : Regex = raw"\(\(\(.*\) - ([0-9]+)\)\)".r
  val SatregNum    : Regex = raw"\(\(\(.*\) ([0-9]+)\)\)".r
  
  def logCommands(flag : Boolean) =
    logCmds = flag

  def sendCommand(cmd : String) : Unit = {
    if (logCmds) {
      println("> " + cmd)
    }
    stdinWriter.println(cmd)
    stdinWriter.flush
  }

  def readLine : String = {
    val ret = stdoutReader.readLine
    if (logCmds) {
      println(">>>" + ret)
    }
    ret
  }

  def declareConst(name : String, typ : String) : Unit =
    sendCommand("(declare-const " + name + " " + typ + ")")

  def freshConst(typ : String) : String = {
    val name = "const_" + nameCounter
    nameCounter = nameCounter + 1
    declareConst(name, typ)
    name
  }

/*
  private var declaredArrays : Map[String, Int] = Map()
  def freshArrayIndex(arrName : String, idx : Int) : String = {
    if(declaredArrays.contains(arrName))
  }
  */

  def addAssertion(str : String) : Unit =
    sendCommand("(assert " + str + ")")

  def push : Unit =
    sendCommand("(push 1)")

  def pop : Unit =
    sendCommand("(pop 1)")

  def isSat : Boolean = {
    sendCommand("(check-sat)")
    readLine match {
      case null =>
        throw new SMTException("solver crashed")
      case "sat" =>
        true
      case "unsat" =>
        false
      case str =>
        throw new SMTException("unexpected answer from solver: " + str)
    }
  }

  def getSatValue(name : String) : BigInt = {
    sendCommand("(get-value (" + name + "))")
    readLine match {
      case SatregNegNum(assignment) => BigInt("-" + assignment)
      case SatregNum(assignment) => BigInt(assignment)
      case NegnumPattern(assignment) => BigInt("-" + assignment)
      case NumberPattern(assignment) => BigInt(assignment)
      case str => 0
    }
  }

  /** DISCLAIMER: This is ugly!!! */
  def getArrayValue(name : String) : Map[BigInt, BigInt] = {
    sendCommand("(get-value (" + name + "))")
    val lineread = readLine 

    // find the number of stores 
    var ret1 = lineread.replaceAll(raw"\(\(as const \(Array Int Int\)\) [0-9]*\)", "")
    val storePattern : Regex = raw"\(store [0-9]* [- 0-9]*\)".r

    var arrayMap : Map[BigInt, BigInt] = Map()

    while (storePattern.findFirstMatchIn(ret1) != None) {
        var tmp = storePattern.findFirstMatchIn(ret1).get.matched.split("\\s+") 
        arrayMap = arrayMap + (BigInt(tmp(1)) -> BigInt(tmp(2).replaceAll(raw"\)", "")))
        ret1 = storePattern.replaceAllIn(ret1, "")
    }

    arrayMap
   }

  def reset : Unit = {
    sendCommand("(reset)")
    nameCounter = 0
    logCmds = false
  }

  def shutdown : Unit = {
    sendCommand("(exit)")
    stdinWriter.close
    stdoutReader.close
    stderr.close
  }

}

class Z3SMT
    extends SMTProcess(Array("z3", "-in")) {
  val name = "Z3"
}

class CVC4SMT
    extends SMTProcess(Array("cvc4", "-i", "--lang=smt")) {
  val name = "CVC4"
  sendCommand("(set-logic ALL)")
}

class PrincessSMT
    extends SMTProcess(Array("princess", "+incremental", "+quiet", "+stdin")) {
  val name = "Princess"
}

