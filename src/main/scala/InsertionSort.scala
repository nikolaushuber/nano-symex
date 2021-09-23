class InsertionSort(A : Program.Var, len : Program.IntConst) {
    import Program._  

    var i = Var("i")
    var j = Var("j")
    var x = Var("x")

    val p = Prog (
        i := 1,
        While(i < len) (
            x := A(i),
            j := i - 1,
            While(j >= 0 & A(j) > x) (
                A(j + 1) := A(j),
                j := j - 1
            ),
            A(j+1) := x,
            i := i + 1
        ),

        i := 0,
        While(i+1 < len) (
            Assert(A(i) <= A(i+1)),
            i := i + 1
        )
    )
}

object InsertionSortTest extends App {
    import Program._ 
    val A = Var("a", PType.PArray) 
    val prog = new InsertionSort(A, IntConst(8)) 

    //println(prog.p)

    import prog._

    val symex = new SymEx(IntExprEncoder, new Z3SMT)
    symex.smt.logCommands(false)
    symex.exec(prog.p, List(A, i, j, x), 200)
    
}
