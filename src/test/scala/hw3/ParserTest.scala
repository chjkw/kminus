package hw3

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * Created with IntelliJ IDEA.
 * User: chjkw
 * Date: 13. 9. 1.
 * Time: 오후 11:34
 * To change this template use File | Settings | File Templates.
 */
class ParserTest extends FunSuite with ShouldMatchers {
  test ("ParseFile fun.k Test") {
    val p = new Parser

    p.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/fun.k-").toString() should be ("[11.1] parsed: LETF(f,List(a, b, c),SEQ(ASSIGN(VAR(b),NUM(0)),ADD(MUL(VAR(a),VAR(b)),MUL(VAR(b),VAR(c)))),LETV(x,NUM(1),LETV(y,NUM(2),LETV(z,NUM(3),SEQ(READ(x),IF(EQUAL(VAR(x),NUM(1)),ADD(CALLV(f,List(VAR(x), VAR(y), VAR(z))),VAR(y)),ADD(CALLR(f,List(x, y, z)),VAR(y))))))))")
  }

  test("test1 test") {
    val p = new Parser
    println(p.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/test1.k-"))
  }

}
