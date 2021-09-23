object arrayParseTest extends App {
    import scala.util.matching.Regex

    val test = "((a (store (store (store ((as const (Array Int Int)) 3) 0 99) 2 45) 1 99)))"


    // find the number of stores 
    var ret1 = test.replaceAll(raw"\(\(as const \(Array Int Int\)\) [0-9]*\)", "")
    val storePattern : Regex = raw"\(store [0-9]* [- 0-9]*\)".r

    var arrayStore = storePattern.findFirstMatchIn(ret1)

    var arrayMap : Map[BigInt, BigInt] = Map()

    while (storePattern.findFirstMatchIn(ret1) != None) {
        var tmp = storePattern.findFirstMatchIn(ret1).get.matched.split("\\s+") 
        arrayMap = arrayMap + (BigInt(tmp(1)) -> BigInt(tmp(2).replaceAll(raw"\)", "")))

        println(arrayMap)

        ret1 = storePattern.replaceAllIn(ret1, "")
    }
}
