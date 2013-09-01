package hw3

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

/**
 * Created with IntelliJ IDEA.
 * User: chjkw
 * Date: 13. 8. 24.
 * Time: 오후 6:46
 */

class KMINUSTest extends FunSuite with ShouldMatchers {

  test("3 ADD 5 test") {
    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, ADD(NUM(2), NUM(3))) should be (5)
  }

  test("5 SUB 3 test") {
    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, SUB(NUM(5), NUM(3))) should be (2)
  }

  test("5 MUL 3 test") {
    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, MUL(NUM(5), NUM(3))) should be (15)
  }

  test("10 DIV 5 test") {
    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, DIV(NUM(10), NUM(5))) should be (2)
  }

  test("TRUE test") {
    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, TRUE) should be (0)
  }

  test("Let x = 5 in write 5 test") {
    val P = LETV("x", NUM(5), WRITE(VAR("x")))
    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P) should be (5)
  }

  test("VAR(x) test") {
    val P = SEQ(SEQ(LETV("x", NUM(5), WRITE(VAR("x"))), ASSIGN("x", NUM(10))), WRITE(VAR("x")))
    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P) should be (10)
  }

  test("WHILE test") {
  }

  test("toy test") {
    val parser = new Parser
    val P = parser.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/test1.k-")

    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P.get) should be (3)
  }


}
