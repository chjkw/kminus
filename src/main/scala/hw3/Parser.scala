package hw3

import scala.util.parsing.combinator.{PackratParsers, JavaTokenParsers}

/** Exceptions occurred in the parser module */
class ParserException(msg: String) extends Exception(msg)

/**
 * Parser for K- language.
 * Basically uses Scala parser combinator and Packrat parser
 *
 * @author jwyoon
 * @since 20130820
 */
class Parser extends JavaTokenParsers with PackratParsers {

  /** Single and multi line comments consideration */
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/|(?m)\(\*(\*(?!\))|[^*])*\*\))+""".r

  /** Grammar for the language */
  lazy val grammar: Parser[EXP] = {
    expression
  }

  /** Rule for an expression */
  lazy val expression: PackratParser[EXP] = {{
    bindingExpression // Starts from
  }}

  /** Rule for a binding expression */
  lazy val bindingExpression: PackratParser[EXP] = {
    "let"~>identifier~":="~expression~"in"~expression ^^ {
      case i~_~e1~_~e2 => LETV(i, e1, e2)
    } | "let"~>"proc"~>identifier~"("~repsep(identifier, ",")~")"~"="~expression~"in"~expression ^^ {
      case i~_~is~_~_~e1~_~e2 => LETF(i, is, e1, e2)
    } | sequentialExpression
  }

  /** Rule for a sequential expression */
  lazy val sequentialExpression: PackratParser[EXP] = {
    controlExpression~";"~sequentialExpression ^^ { case e1~_~e2 => SEQ(e1, e2) } |
    controlExpression // Go to the next expression
  }

  /** Rule for a control expression */
  lazy val controlExpression: PackratParser[EXP] = {
    "if"~>expression~"then"~expression~"else"~expression ^^ {
      case e1~_~e2~_~e3 => IF(e1, e2, e3)
    } | "while"~>expression~"do"~expression ^^ {
      case e1~_~e2 => WHILE(e1, e2)
    } | assignmentExpression
  }

  /** Rule for an assignment expression */
  lazy val assignmentExpression: PackratParser[EXP] = {
    ioExpression~":="~assignmentExpression ^^ {
      case e1~_~e2 => ASSIGN(e1, e2)
    } | ioExpression // Go to the next expression
  }

  /** Rule for an input/output expression */
  lazy val ioExpression: PackratParser[EXP] = {
    "read"~>identifier ^^ {
      case i => READ(i)
    } | "write"~>ioExpression ^^ {
      case e => WRITE(e)
    } | relationalExpression
  }

  /** Rule for a relational(<, =) expression */
  lazy val relationalExpression: PackratParser[EXP] = {
    relationalExpression~"<"~additiveExpression ^^ {
      case e1~_~e2 => LESS(e1, e2)
    } | relationalExpression~"="~additiveExpression ^^ {
      case e1~_~e2 => EQUAL(e1, e2)
    } | additiveExpression
  }

  /** Rule for an additive(+, -) expression */
  lazy val additiveExpression: PackratParser[EXP] = {
    additiveExpression~"+"~multiplicativeExpression ^^ {
      case e1~_~e2 => ADD(e1, e2)
    } | additiveExpression~"-"~multiplicativeExpression ^^ {
      case e1~_~e2 => SUB(e1, e2)
    } | multiplicativeExpression // Go to the next expression
  }

  /** Rule for a multiplicative(*, /) expression */
  lazy val multiplicativeExpression: PackratParser[EXP] = {
    multiplicativeExpression~"*"~unaryExpression ^^ {
      case e1~_~e2 => MUL(e1, e2)
    } | multiplicativeExpression~"/"~unaryExpression ^^ {
      case e1~_~e2 => DIV(e1, e2)
    } | unaryExpression // Go to the next expression
  }

  /** Rule for an unary(not) expression */
  lazy val unaryExpression: PackratParser[EXP] = {
    "not"~>unaryExpression ^^ {
      case e => NOT(e)
    } | callExpression
  }

  /** Rule for a call expression */
  lazy val callExpression: PackratParser[EXP] = {
    identifier~"("~repsep(expression, ",")<~")" ^^ {
      case i~_~es => CALLV(i, es)
    } | identifier~"<"~repsep(identifier, ",")<~">" ^^ {
      case i~_~is => CALLR(i, is)
    } | parenthesizedExpression
  }

  /** Rule for a parenthesized expression */
  lazy val parenthesizedExpression: PackratParser[EXP] = {
    "("~>expression<~")" ^^ { case e => e } |
    recordListExpression // Go to the next expression
  }

  /** Rule for a record list expression */
  lazy val recordListExpression: PackratParser[EXP] = {
    "{"~>repsep(singleRecordExpression, ",")<~"}" ^^ {
      case rs => RECORDS(rs)
    } | recordLookupExpression
  }

  /** Rule for a single record expression */
  lazy val singleRecordExpression: PackratParser[RECORD] = {
    identifier~":="~expression ^^ {
      case i~_~e => RECORD(i, e)
    }
  }

  /** Rule for a record lookup expression */
  lazy val recordLookupExpression: PackratParser[EXP] = {
    recordLookupExpression~"."~identifier ^^ {
      case e~_~i => FIELD(e, i)
    } | primaryExpression
  }

  /** Rule for a primary expression */
  lazy val primaryExpression: Parser[EXP] = {
    identifierExpression |
    booleanValue |
    integerValue |
    unitExpression
  }

  /** Rule for an identifier expression which contains an identifier */
  lazy val identifierExpression: Parser[EXP] = {
    identifier ^^ { case i => VAR(i) }
  }

//  /** Rule for a constant expression which contains a value */
//  lazy val constantExpression: Parser[EXP] = {
//    value ^^ { case b => NUM(b) }
//  }

  /** Rule for an unit expression */
  lazy val unitExpression: Parser[EXP] = {
    "" ^^ { case _ => UNIT }
  }

  /** Rule for a value */
  lazy val value: Parser[EXP] = {
    integerValue |
    booleanValue
  }

  /** Rule for an integer value */
  lazy val integerValue: Parser[NUM] = {
    """[-]?[0-9]+""".r ^^ { case i => NUM(i.toInt) }
  }

  /** Rule for a boolean value */
  lazy val booleanValue: Parser[EXP] = {
    "true" ^^ { case _ => TRUE } | // True value
    "false" ^^ { case _ => FALSE } // False value
  }

  /** Rule for an identifier */
  lazy val identifier: Parser[KMINUS.ID] = {
    """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ { case s => s.toString }
  }

  /**
   * Parse the given content
   * @param content the target content
   * @return the parsed result
   */
  def parse(content: String): ParseResult[EXP] = {
    try {
      parseAll(grammar, content)
    } catch {
      case e: Throwable => {
        throw new ParserException("Parsing error: " + e.getMessage)
      }
    }
  }

  /**
   * Parse from the given file name
   * @param fileName the target file name
   * @return the parsed result
   */
  def parseFile(fileName: String): ParseResult[EXP] = {
    import scala.io.Source
    import scala.util.control.Exception.catching
    val sourceOpt = catching(classOf[Throwable]) opt Source.fromFile(fileName,"UTF-8")
    if(sourceOpt.nonEmpty) {
      val source = sourceOpt.get
      try {
        parseAll(grammar, source.bufferedReader())
      } catch {
        case e: Throwable => {
          throw new ParserException("Parsing error: " + e.getMessage)
        }
      } finally {
        source.close()
      }
    } else {
      throw new ParserException("The source file cannot be parsed")
    }
  }
}
