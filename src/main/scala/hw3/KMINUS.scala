package hw3

import com.sun.corba.se.impl.io.TypeMismatchException

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

abstract class EnvEntry
case class Addr(addr: Int) extends EnvEntry
case class Proc(params :List[KMINUS.ID], b: EXP, e: KMINUS.Env) extends EnvEntry

object Addr {
  private var addr = 1

  def increase() : Addr = {
    addr = addr + 1
    Addr(addr)
  }
}

object KMINUS {
  var addr: Addr = new Addr(1)

  type Error = String
  type ID = String

  type PROGRAM = EXP
  type Memory = Map[EnvEntry, Value]
  type Env = Map[ID, EnvEntry]

  val emptyMemory: Memory = Map.empty[EnvEntry, Value]
  val emptyEnv: Env = Map.empty[ID, EnvEntry]

  def calc(M: Memory, E: Env, e1: EXP, e2: EXP, op: Char) : (Memory, Env, Value) = {
    val r1 = runExpr(M, E, e1)
    val r2 = runExpr(r1._1, r1._2, e2)

    (r1._3,r2._3) match {
      case (Numv(n1),Numv(n2)) => {
        op match {
          case '+' => (r2._1, r2._2, Numv(n1 + n2))
          case '-' => (r2._1, r2._2, Numv(n1 - n2))
          case '*' => (r2._1, r2._2, Numv(n1 * n2))
          case '/' => {
            if(n2 == 0)
              throw new ArithmeticException
            else
              (r2._1, r2._2, Numv(n1 / n2))
          }
        }
      }
      case _ => throw new TypeMismatchException
    }
  }

  def run(M: Memory, E: Env, P: PROGRAM): Value = {
    val e = runExpr(M, E, P)
    e._3
  }

  def runExpr(M: Memory, E: Env, P: PROGRAM): (Memory, Env, Value) = {
    P match {
      case VAR(x) => (M, E, M(E(x)))
      case TRUE => (M, E, Bool(true))
      case FALSE => (M, E, Bool(false))
      case UNIT => (M, E, Unitv)
      case NUM(n) => (M, E, Numv(n))
      case ADD(p1, p2) => calc(M, E, p1, p2, '+')
      case SUB(p1, p2) => calc(M, E, p1, p2, '-')
      case MUL(p1, p2) => calc(M, E, p1, p2, '*')
      case DIV(p1, p2) => calc(M, E, p1, p2, '/')
      case EQUAL(e1, e2) => if (run(M, E, e1) == run(M, E, e2)) (M, E, Bool(true)) else (M, E, Bool(false))
      case LESS(e1, e2) => {
        val r1 = runExpr(M, E, e1)
        val r2 = runExpr(r1._1, r1._2, e2)

        (r1._3, r2._3) match {
          case (Numv(n1), Numv(n2)) => if(n1 < n2) (r2._1, r2._2, Bool(true)) else (r2._1, r2._2, Bool(false))
          case _ => throw new TypeMismatchException
        }
      }
      case NOT(e) => {
        val r = runExpr(M, E, e)

        r._3 match {
          case Bool(b) => if(b) (r._1, r._2, Bool(false)) else (r._1, r._2, Bool(true))
          case _ => throw new TypeMismatchException
        }
      }
      case SEQ(e1, e2) => {
        val r = runExpr(M, E, e1)
        runExpr(r._1, r._2, e2)
      }
      case IF(cond, t, e) => {
        val r = runExpr(M, E, cond)
        r._3 match {
          case Bool(b) => if (b == true) runExpr(r._1, r._2, t) else runExpr(r._1, r._2, e)
          case _ => runExpr(r._1, r._2, e)
        }
      }
      case WHILE(cond, s) => {
        val m = runExpr(M, E, cond)
        m._3 match {
          case Bool(b) if b == true => {
            val r = runExpr(m._1, m._2, s)
            runExpr(r._1, r._2, WHILE(cond, s))
          }
          case _ => (m._1, m._2, Unitv)
        }
      }
      case ASSIGN(id, v) => (M.updated(E(id), runExpr(M, E, v)._3), E, Unitv)
      case LETV(id, e, s) => {
        val x = runExpr(M, E, e)._3
        val l = Addr.increase()

        runExpr(M + (l -> x), E + (id -> l), s)
      }
      case LETF(id, params, b, s) => {
        runExpr(M, E + (id -> new Proc(params, b, E)), s)
      }
      case CALLV(id, args) =>
      {
        val f = E(id)

        f match {
          case Proc(params, b, e) => {
            // traverse params
            val (mem,env) = params.zip(args).foldLeft(M,E) {
              case ((m,e),l) =>
                val env = e + (l._1 -> Addr.increase())
                val mem = m + (env(l._1) -> run(M ++ m, E, l._2))
                (mem,env)
            }
            // run the function body under the Memory and the Env
            val r = runExpr(M ++ mem, E ++ e ++ env, b)

            // return the value with the original Memory and Env
            (r._1, E, r._3)
          }
          case _ => throw new TypeMismatchException
        }
      }

      case CALLR(id, args) =>
      {
        val f = E(id)

        f match {
          case Proc(params, b, e) => {
            // traverse params and use the original address of each parameter
            val env = params.zip(args).foldLeft(e ++ E) {
              case(env, l) => env + (l._1 -> E(l._2))
            }

            // run the function body under the Memory and the Env
            val r = runExpr(M, E ++ env ++ e, b)

            // return the value with the original Memory and Env
            (r._1, E, r._3)
          }
          case _ => throw new TypeMismatchException
        }
      }
      case RECORD(id, e) => ???
      case RECORDS(l) => ???
      case FIELD(e, id) => ???
      case ASSIGNF(r, id, v) => ???

      case READ(id) => {
        val x = readInt()

        if(E(id) != E.empty){
          val r = runExpr(M, E, ASSIGN(id, NUM(x)))
          (r._1, r._2, Numv(x))
        } else {
          val l = Addr.increase()
          (M + (l -> Numv(x)), E + (id -> l), Numv(x))
        }
      }
      case WRITE(e) => {
        val r = runExpr(M, E, e)
        r._3 match {
          case Numv(n) => println(n)
          case _ => println()
        }
        r
      }
    }
  }
}






