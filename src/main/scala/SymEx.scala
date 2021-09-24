

/**
 * Extremely simple symbolic execution engine.
 */
class SymEx(encoder : ExprEncoder, spawnSMT : => SMT) {

  val smt = spawnSMT

  import encoder._
  import Program._
  import PType.{PInt, PArray}
  import smt._

  def shutdown = smt.shutdown

  smt.logCommands(false)

  def exec(p : Prog, variables : Seq[Var], depth : Int = Integer.MAX_VALUE) = {
    // Only need to declare the integer constants
    for (Var(name, PInt) <- variables) 
      declareConst(name, IntType) 

    // In the normal store, we only store simple Int variables
    val store =
      (for (v@Var(name, PInt) <- variables) yield (v -> name)).toMap

    // In the array store, we keep all the array variables
    val arrayStore : SymbArrayStore = 
      (for (a@Var(name, PArray) <- variables) yield (a -> Map[BigInt, String]())).toMap

    execHelp(p, List(), depth)(store, arrayStore)

    reset
  }

  def execHelp(p : Prog, ops : List[Prog], depth : Int)
              (implicit store : SymbStore, arrayStore : SymbArrayStore) : Unit = p match {

    case _ if ops.size > depth => ()

    case Skip => ()

    case Sequence(Skip, rest) =>
      execHelp(rest, ops, depth)

    case Sequence(Sequence(p1, p2), p3) =>
      execHelp(Sequence(p1, Sequence(p2, p3)), ops, depth)

    

    case Sequence(op@Assign(lhs : Var, rhs), rest) => {
      // We need to distinguish between two cases, rhs being 
      // an expression, and rhs being an array access
      rhs match {
        case arr@ArrayElement(a, i) => {
          if(isSat) {
            val nConst = freshConst(IntType)
            // the index might be given as an expression, not a simple Int
            val idx : BigInt = getSatValue(encode(i)) 
            // Add assertion that the index of the array element we are 
            // looking at corresponds to the value of the expression
            val cond = "(= " + encode(i) + " " + idx + ")" 

            push // branch for idx == encode(i)
            addAssertion(cond) 

            // Update the symbolic array store if necessary 
            val newArrayStore = 
              if (!arrayStore(a).contains(idx)) {
                //The array a does not contain a value for the current index yet
                val newVal = freshConst(IntType)
                arrayStore + (a -> (arrayStore(a) + (idx -> newVal)))
              } else 
                arrayStore 

            // This will pick up the new variable just created in case 
            // this is the first access to this specific index in the array
            val currArrayVar = newArrayStore(a)(idx)
            addAssertion("(= " + nConst + " " + currArrayVar + ")")
            val newStore = store + (lhs -> nConst)
            execHelp(rest, op :: ops, depth)(newStore, newArrayStore) 
            pop
            push // branch for idx != encode(i)
            addAssertion("(not " + cond + ")") 
            execHelp(Sequence(op, rest), op :: ops, depth)(store, arrayStore)
            pop
          }
        }

        case _ => {
          val newConst = freshConst(IntType)
          addAssertion("(= " + newConst + " " + encode(rhs) + ")")
          // If lhs already exists in store, it will be overwritten! 
          val newStore = store + (lhs -> newConst)
          execHelp(rest, op :: ops, depth)(newStore, arrayStore)
        }
      }
      
    }

    
    case Sequence(op@Assign(ArrayElement(a, i), rhs), rest) => {
      // Since we only analyze normalized programs, rhs cannot be an array access

      if(isSat) { // allows us to use getSatValue for index calculation
        // i might be an expression and not a simple Int 
        val idx = getSatValue(encode(i))
        val nConst = freshConst(IntType)
        // if there is already a value at index idx, we need to reassign 
        // it to the new constant nConst
        val newArrayStore = arrayStore + (a -> (arrayStore(a) + (idx -> nConst)))
        // add assertion that the index coincides with the value of the expression 
        val cond = "(= " + encode(i) + " " + idx + ")"

        push // branch for idx == encode(i)
        // add assertion that the new array element equals the value of rhs
        addAssertion("(= " + nConst + " " + encode(rhs) + ")")
        addAssertion(cond)
        execHelp(rest, op :: ops, depth)(store, newArrayStore)
        pop
        push // branch for idx != encode(i)
        addAssertion("(not " + cond + ")")
        execHelp(Sequence(op, rest), op :: ops, depth)(store, arrayStore) 
        pop 
      }
    }
    

    case Sequence(IfThenElse(cond, b1, b2), rest) => {
      val condStr = encode(cond)
      push
      addAssertion(condStr)
      val trueBranchSat = isSat
      if (trueBranchSat)
        execHelp(Sequence(b1, rest), ops, depth)
      pop
      push
      addAssertion("(not " + condStr + ")")
      if (!trueBranchSat || isSat)
        execHelp(Sequence(b2, rest), ops, depth)
      pop
    }

    case Sequence(w@While(cond, body), rest) =>
      execHelp(Sequence(IfThenElse(!cond, Skip, Sequence(body, w)), rest),
               ops, depth)

    case Sequence(a@Assert(cond), rest) => {
      push
      addAssertion(encode(!cond))
      if (isSat) {
        println("Found path leading to failing assertion:")
        for (op <- (a :: ops).reverse)
          println("  " + op)
      }
      pop
      execHelp(rest, ops, depth)
    }

    case p =>
      execHelp(Sequence(p, Skip), ops, depth)

  }

}


object SymExTest extends App {

  import ExampleProg._

  val symex = new SymEx(IntExprEncoder, new Z3SMT)

  symex.exec(p, List(a, b, x, y))

}

object SymExTest2 extends App {

  import ExampleProg2._

  val symex = new SymEx(IntExprEncoder, new Z3SMT)

  symex.exec(p, List(a, x), 200)

}
