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

    // println(p.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/fun.k-").get)
  }

  test("test1 test") {
    val p = new Parser
    // println(p.parseFile("/Users/chjkw/dev/scala/pl/hw/hw3/examples/test1.k-").get)
  }

}
