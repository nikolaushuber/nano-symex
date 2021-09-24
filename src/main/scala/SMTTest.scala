

object SMTTest extends App {

  for (smt <- List(new Z3SMT))
  try {
    import smt._
    println("Testing SMT solver " + name + " ...")

    declareConst("x", "Int")
    declareConst("y", "Int")

    addAssertion("(> x y)")
    println(isSat)
    if(isSat) println("x: " + getSatValue("x") + ", y: " + getSatValue("y"))

  } finally {
    smt.shutdown
  }

}

object SMTArrayTest extends App {
  val smt = new Z3SMT 
  try {
    import smt._ 
    println("Testing SMT solver " + name + " with arrays ...")

    declareConst("a", "(Array Int Int)")

    addAssertion("(= a (store a 0 11))")
    addAssertion("(= a (store a 1 12))")
    addAssertion("(= a (store a 2 13))")

    if(isSat) println("a: " + getArrayValue("a"))
  } finally {
    smt.shutdown
  }
}
