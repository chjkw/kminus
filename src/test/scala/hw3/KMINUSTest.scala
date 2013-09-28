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
  test("Add test") {
    val parser = new Parser
    val P = parser.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/add.k-")

    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P.get) should be (Numv(3))
  }

  test("Sub test") {
    val parser = new Parser
    val P = parser.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/sub.k-")

    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P.get) should be (Numv(1))
  }

  test("Mul test") {
    val parser = new Parser
    val P = parser.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/mul.k-")

    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P.get) should be (Numv(2))
  }

  test("Div test") {
    val parser = new Parser
    val P = parser.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/div.k-")

    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P.get) should be (Numv(2))
  }

  test("Div by Zero test") {
    val parser = new Parser
    val P = parser.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/div_zero.k-")

    evaluating {KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P.get)} should produce [ArithmeticException]
  }

  test("WHILE test") {
    val parser = new Parser
    val P = parser.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/while.k-")

    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P.get) should be (Unitv)
  }

  test("ASSIGN test") {
    val parser = new Parser
    val P = parser.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/assign.k-")

    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P.get) should be (Numv(2))
  }

  test("Call by Value test") {
    val parser = new Parser
    val P = parser.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/callv.k-")

    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P.get) should be (Numv(2))
  }

  test("Call by Reference test") {
    val parser = new Parser
    val P = parser.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/callr.k-")

    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P.get) should be (Numv(0))
  }

  test("IF test") {
    val parser = new Parser
    val P = parser.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/if.k-")

    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P.get)
  }

  test("Record Creation test") {
    val parser = new Parser
    val P = parser.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/rec.k-")

    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P.get) should be (Numv(265))
  }

  test("Assign to Field test") {
    val parser = new Parser
    val P = parser.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/assignf.k-")

    KMINUS.run(KMINUS.emptyMemory, KMINUS.emptyEnv, P.get) should be (Numv(3))
  }
}
