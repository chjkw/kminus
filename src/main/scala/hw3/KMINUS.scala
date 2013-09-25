package hw3

import com.sun.corba.se.impl.io.TypeMismatchException
import scala.annotation.tailrec

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

abstract class Value
case object Unitv extends Value
case class Numv(n: Int) extends Value
case class Bool(b: Boolean) extends Value
case class Recordv(r: KMINUS.ID => Int) extends Value

object KMINUS {
  private var addr: Int = 1
  type Error = String
  type ID = Any

  type PROGRAM = EXP
  type Memory = Map[Any, Value]
  type Env = Map[ID, Any]

  val emptyMemory: Memory = Map.empty[Any, Value]
  val emptyEnv: Env = Map.empty[ID, Any]

  def getAddr() : Int = { this.addr+=1; this.addr }

  def _run(M: Memory, E: Env, P: PROGRAM): (Memory, Env, Value) = {
    P match {
      case VAR(x) => (M, E, M(E(x)))
      case TRUE => (M, E, Bool(true))
      case FALSE => (M, E, Bool(false))
      case UNIT => (M, E, Unitv)
      case NUM(n) => (M, E, Numv(n))
      case ADD(p1, p2) => {
        val r1 = _run(M, E, p1)
        val r2 = _run(r1._1, r1._2, p2)

        r1._3 match {
          case Numv(n1) => {
            r2._3 match {
              case Numv(n2) => (r2._1, r2._2, Numv(n1 + n2))
            }
          }
          case _ => throw new TypeMismatchException

        }
      }
      case SUB(p1, p2) => {
        val r1 = _run(M, E, p1)
        val r2 = _run(r1._1, r1._2, p2)

        r1._3 match {
          case Numv(n1) => {
            r2._3 match {
              case Numv(n2) => (r2._1, r2._2, Numv(n1 - n2))
            }
          }
          case _ => throw new TypeMismatchException

        }
      }
      case MUL(p1, p2) => {
        val r1 = _run(M, E, p1)
        val r2 = _run(r1._1, r1._2, p2)

        r1._3 match {
          case Numv(n1) => {
            r2._3 match {
              case Numv(n2) => (r2._1, r2._2, Numv(n1 * n2))
            }
          }
          case _ => throw new TypeMismatchException

        }
      }
      case DIV(p1, p2) => {
        val r1 = _run(M, E, p1)
        val r2 = _run(r1._1, r1._2, p2)

        r1._3 match {
          case Numv(n1) => {
            r2._3 match {
              case Numv(n2) => {
                if(n2 == 0) throw new ArithmeticException
                else (r2._1, r2._2, Numv(n1 / n2))
              }
            }
          }
          case _ => throw new TypeMismatchException
        }
      }
      case EQUAL(e1, e2) => if (run(M, E, e1) == run(M, E, e2)) (M, E, Bool(true)) else (M, E, Bool(false))
      case LESS(e1, e2) => {
        val r1 = _run(M, E, e1)
        val r2 = _run(r1._1, r1._2, e2)

        r1._3 match {
          case Numv(n1) =>
            r2._3 match {
              case Numv(n2) => if(n1 < n2) (r2._1, r2._2, Bool(true)) else (r2._1, r2._2, Bool(false))
            }
          case _ => throw new TypeMismatchException
        }
      }
      case NOT(e) => {
        val r = _run(M, E, e)

        r._3 match {
          case Bool(b) => if(b) (r._1, r._2, Bool(false)) else (r._1, r._2, Bool(true))
        }
      }
      case SEQ(e1, e2) => {
        val r = _run(M, E, e1)
        _run(r._1, r._2, e2)
      }
      case IF(cond, t, e) => {
        val r = _run(M, E, cond)
        r._3 match {
          case Bool(b) => if (b == true) _run(r._1, r._2, t) else _run(r._1, r._2, e)
          case _ => _run(r._1, r._2, e)
        }
      }
      case WHILE(cond, s) => {
        val m = _run(M, E, cond)
        m._3 match {
          case Bool(b) => {
            if (b == true) {
              val r = _run(m._1, m._2, s)
              _run(r._1, r._2, WHILE(cond, s))
            }
            else
              (m._1, m._2, Unitv)
          }
          case _ => (m._1, m._2, Unitv)
        }
      }
      case ASSIGN(id, v) => (M.updated(E(id), _run(M, E, v)._3), E, Unitv)
      case LETV(id, e, s) => {
        val x = _run(M, E, e)._3
        val l = getAddr()

        _run(M + (l -> x), E + (id -> l), s)
      }
      case LETF(id, args, b, s) => {
        _run(M, E + (id -> (args, b, E)), s)
      }
      case CALLV(id, args) => ???
      case CALLR(id, args) => ???
      case RECORD(id, e) => ???
      case RECORDS(l) => ???
      case FIELD(e, id) => ???
      case ASSIGNF(r, id, v) => ???

      case READ(id) => {
        val x = readInt()

        if(E(id) != E.empty){
          val r = _run(M, E, ASSIGN(id, NUM(x)))
          (r._1, r._2, Numv(x))
        } else {
          val l = getAddr()
          (M + (l -> Numv(x)), E + (id -> l), Numv(x))
        }
      }
      case WRITE(e) => {
        val r = _run(M, E, e)
        println(r._3)
        r
      }
    }
  }

  def run(M: Memory, E: Env, P: PROGRAM): Value = {
    val e = _run(M, E, P)
    e._3
  }
}






