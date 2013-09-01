package hw3

/**
 * Created with IntelliJ IDEA.
 * User: chjkw
 * Date: 13. 8. 22.
 * Time: 오후 7:18
 */


abstract class EXP
case object TRUE extends EXP
case object FALSE extends EXP
case object UNIT extends EXP
case class NUM(n: Int) extends EXP
case class VAR(id: KMINUS.ID) extends EXP
case class ADD(e1: EXP, e2: EXP) extends EXP
case class SUB(e1: EXP, e2: EXP) extends EXP
case class MUL(e1: EXP, e2: EXP) extends EXP
case class DIV(e1: EXP, e2: EXP) extends EXP
case class EQUAL(e1: EXP, e2: EXP) extends EXP
case class LESS(e1: EXP, e2: EXP) extends EXP
case class NOT(e: EXP) extends EXP
case class SEQ(e1: EXP, e2: EXP) extends EXP
case class IF(cond: EXP, t: EXP, e: EXP) extends EXP
case class WHILE(cond: EXP, s: EXP) extends EXP
case class LETV(id: KMINUS.ID, e: EXP, s:EXP) extends EXP
case class LETF(id: KMINUS.ID, args: List[KMINUS.ID], b: EXP, s: EXP) extends EXP
case class CALLV(id: KMINUS.ID, args: List[EXP]) extends EXP
case class CALLR(id: KMINUS.ID, args: List[KMINUS.ID]) extends EXP
case class RECORD(id: KMINUS.ID, e: EXP) extends EXP
case class RECORDS(l: List[RECORD]) extends EXP
case class FIELD(e: EXP, id: KMINUS.ID) extends EXP
case class ASSIGN(id: KMINUS.ID, v: EXP) extends EXP
case class ASSIGNF(r: EXP, id: KMINUS.ID, v: EXP) extends EXP
case class READ(id: KMINUS.ID) extends EXP
case class WRITE(e: EXP) extends EXP

object KMINUS {
  private var addr: Int = 1
  type Error = String
  type ID = Any

  type PROGRAM = EXP
  type Memory = Map[Any, EXP]
  type Env = Map[ID,  Any]

  object Value

  val emptyMemory: Memory = Map.empty[Any, EXP]
  val emptyEnv: Env = Map.empty[ID, Any]

  def getAddr() : Int = { this.addr+=1; this.addr }

  def _run(M: Memory, E: Env, P: PROGRAM): (Memory, Env, EXP) = {
    P match {
      case VAR(x) => (M, E, M(E(x)))
      case TRUE => (M, E, TRUE)
      case FALSE => (M, E, FALSE)
      case UNIT => (M, E, UNIT)
      case NUM(n) => (M, E, NUM(n))
      case ADD(p1, p2) => (M, E, NUM(run(M, E, p1) + run(M, E, p2)))
      case SUB(p1, p2) => (M, E, NUM(run(M, E, p1) - run(M, E, p2)))
      case MUL(p1, p2) => (M, E, NUM(run(M, E, p1) * run(M, E, p2)))
      case DIV(p1, p2) => (M, E, NUM(run(M, E, p1) / run(M, E, p2)))
      case EQUAL(e1, e2) => if (run(M, E, e1) == run(M, E, e2)) (M, E, TRUE) else (M, E, FALSE)
      case LESS(e1, e2) => if (run(M, E, e1) < run(M, E, e2)) (M, E, TRUE) else (M, E, FALSE)
      case NOT(e) => if(_run(M, E, e) == TRUE) (M, E, FALSE) else (M, E, TRUE)
      case SEQ(e1, e2) => {
        val r = _run(M, E, e1)
        _run(r._1, r._2, e2)
      }
      case IF(cond, t, e) => if(_run(M, E, cond)._3 == TRUE) _run(M, E, t) else _run(M, E, e)
      case WHILE(cond, s) => {
        if(_run(M, E, cond)._3 == TRUE) {
        val r = _run(M, E,s)
          _run(r._1, r._2, WHILE(cond, s))
        } else {
          (M, E, UNIT)
        }
      }
      case ASSIGN(id, v) => (M.updated(E(id), v), E, UNIT)
      case LETV(id, e, s) => {
        val x = run(M, E, e)
        val l = getAddr()

        _run(M + (l -> NUM(x)), E + (id -> l), s)
      }
      case LETF(id, args, b, s) => {
        _run(M, E + (id -> (args, b, E)), s)
      }
      case READ(id) => {
        val x = readInt()
        val l = getAddr()

        (M + (l -> NUM(x)), E + (id -> l), NUM(x))
      }
      case WRITE(e) => {
        val r = run(M, E, e)
        println(r)
        (M, E, NUM(r))
      }
    }
  }

  def run(M: Memory, E: Env, P: PROGRAM): Int = {
    val e = _run(M, E, P)

    e._3 match {
      case NUM(n) => n
      case _ => 0
    }
  }
}






