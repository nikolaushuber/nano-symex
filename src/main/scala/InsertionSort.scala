class InsertionSort(A : Program.Var, len : Program.IntConst) {
    import Program._  

    var i = Var("i")
    var j = Var("j")
    var x = Var("x")
    var aj = Var("Aj")
    var ai = Var("Ai")
    var ai1 = Var("Ai1")

    val p = Prog (
        i := 1,
        While(i < len) (
            x := A(i),
            j := i - 1,
            aj := A(j), 
            While(j >= 0 & aj > x) (
                A(j + 1) := aj,
                j := j - 1,
                aj := A(j)
            ),
            A(j+1) := x,
            i := i + 1
        ),

        i := 0,
        While(i+1 < len) (
            ai := A(i),
            ai1 := A(i+1),
            Assert(ai <= ai1),
            i := i + 1
        )
    )
}

object InsertionSortTest extends App {
    import Program._ 
    val A = Var("a", PType.PArray) 
    val prog = new InsertionSort(A, IntConst(7)) 

    //println(prog.p)

    import prog._

    val symex = new SymEx(IntExprEncoder, new Z3SMT)
    symex.smt.logCommands(false)
    symex.exec(prog.p, List(A, i, j, x, aj, ai, ai1), 200)
    
}
