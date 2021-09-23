object _ArrayTest1 {
    import Program._ 

    val x = Var("x")
    val y = Var("y")
    val a = Var("a", PType.PArray)

    val p = Prog(
        ArrayElement(a, 0) := 0,
        ArrayElement(a, 1) := 1,
        x := ArrayElement(a, 0),
        y := ArrayElement(a, 1),
        Assert(x <= y)
    )
}

object ArrayTest1 extends App {
    println(_ArrayTest1.p)
    //val symex = new SymEx(IntExprEncoder, new Z3SMT)
}
