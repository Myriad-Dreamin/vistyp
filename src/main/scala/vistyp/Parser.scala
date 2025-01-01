package vistyp

import scala.util.chaining._
import scala.util.TupledFunction

import fastparse._, fastparse.ScalaWhitespace._

import vistyp.syntax._
import vistyp.syntax.NodeParse._

implicit class SoftErr[$: P](p: => P[Unit]) {
  def err(msg: String): P[Unit] = p.map(_ => Err(msg))
}

def parseCode(src: String): syntax.MarkupBlock = {
  fastparse.parse(src, Parser.markupRoot(_)) match {
    case Parsed.Success(ast, _) => ast
    case Parsed.Failure(_, index, extra) =>
      println(extra.trace().longAggregateMsg)
      println(src.slice(index, index + 40))
      throw new Exception("Parsing failed")
  }
}

object Parser {
  // Entry point
  def markupRoot[$: P]: P[MarkupBlock] =
    P(("" ~ markupExpr).rep.map(_.toList) ~ End).map(MarkupBlock.apply).m
  def markupMode[$: P]: P[MarkupBlock] =
    P(("" ~ markupExpr).rep.map(_.toList)).map(MarkupBlock.apply).m
  def codeMode[$: P]: P[Block] =
    P(("" ~ codeExpr).rep.map(_.toList)).map(Block.apply).m

  def markupExpr[$: P]: P[Node] = P(
    hashExpr,
  )

  def hashExpr[$: P]: P[Node] = P("#" ~ hashExprBody)

  def hashExprBody[$: P]: P[Node] = P(
    codeBlock | contentBlock | letBinding,
  )

  def codeBlock[$: P]: P[Node] = P(
    "{" ~/ codeMode ~ "}",
  )

  def contentBlock[$: P]: P[Node] = P(
    "[" ~/ markupMode ~ "]",
  )

  def codeExprCommon[$: P]: P[Node] = P(
    letBinding | funcCall | ifStmt | parens | assign,
  )
  def lazyCodeExpr[$: P]: P[Node] = P(
    lazyCodeBlock | lazyContentBlock | codeExprCommon,
  )

  def codeExpr[$: P]: P[Node] = P(
    codeBlock | contentBlock | codeExprCommon,
  )

  def lazyCodeBlock[$: P]: P[Node] = codeBlock
  def lazyContentBlock[$: P]: P[Node] = contentBlock

  def letBinding[$: P]: P[Node] = P(
    "let" ~/ ident ~/ paramList.? ~ "=" ~/ lazyCodeExpr,
  ).map(LetBinding.apply.tupled)

  def paramList[$: P]: P[List[Node]] = P(
    "(" ~/ (ident ~ defaultValue.?).map(Param.apply.tupled).rep(sep = ",") ~ ")",
  ).map(_.toList)
  def defaultValue[$: P] = P(":" ~/ codeExpr).m

  def funcCall[$: P]: P[Node] = P(ident ~ argList).map(Apply.apply.tupled)
  def argList[$: P]: P[List[Node]] = P(
    "(" ~/ (namedArg | spreadArg | codeExpr).rep(sep = ",") ~ ")",
  ).map(_.toList)

  def namedArg[$: P]: P[Node] = P(
    ident ~ ":" ~/ codeExpr,
  ).map(KeyedArg.apply.tupled)

  def spreadArg[$: P]: P[Node] = P(".." ~/ codeExpr).map(UnOp("..", _))

  def anyBlock[$: P]: P[Node] = P(
    codeBlock | contentBlock,
  )

  def ifStmt[$: P]: P[Node] = P(
    "if" ~/ codeExpr ~ anyBlock ~ ("else" ~ (anyBlock | ifStmt)).?,
  ).map(If.apply.tupled)

  // Literals
  def noneLit[$: P] = P("none").map(_ => NoneLit)
  def autoLit[$: P] = P("auto").map(_ => AutoLit)
  def booleanLit[$: P] =
    (P(word("true") | word("false"))).!.map(v => BoolLit(v == "true"))
  def numberLit[$: P] = P(float.map(FloatLit.apply) | int.map(IntLit.apply))
  def stringLit[$: P] = P(longStr | shortStr).map(StrLit.apply)

  // Lexer
  def word[$: P](s: String) = s ~~ !idCont
  def letter[$: P] = P(lowercase | uppercase)
  def lowercase[$: P] = P(CharIn("a-z"))
  def uppercase[$: P] = P(CharIn("A-Z"))
  def digit[$: P] = P(CharIn("0-9"))
  def id[$: P] = P((letter | "_") ~~ idCont.repX).!.filter(!keywords(_))
  def idCont[$: P] = P(letter | digit | "_" | "-")

  def delimStr[T, $: P](d: String, body: => P[T]) = d ~~/ delimStrCont(d, body)
  def delimStrCont[T, $: P](d: String, body: => P[T]) =
    (!End ~~ !d ~~ body).repX.! ~~ (d | End.err("Unclosed string"))
  def shortStr[$: P] = P(shortStr0.map(unescapeStr))
  def shortStr0[$: P] = delimStr("\"", strEscape)
  def longStr[$: P] = P("\"".repX(3).!./.flatMapX(longStr0))
  def longStr0[$: P](delim: String) = P(delimStrCont(delim, longStrBody(delim)))
  def strEscape[$: P] = P(litCharsWhile("\\\n\"") | escapeseq)
  def longStrBody[$: P](delim: String) = P(longChars("\"", delim).repX.!)

  def longChars[$: P](mores: String, delim: String): P[Unit] =
    !End ~~ P(litCharsWhile(mores) | !delim ~~ "\"".repX)
  def litCharsWhile[$: P](mores: String) = P(CharsWhile(!mores.contains(_)))
  def escapeseq[$: P]: P[Unit] = P("\\" ~ AnyChar)

  def int[$: P]: P[BigInt] =
    P(octinteger | hexinteger | bininteger | decimalinteger)
  def decimalinteger[$: P]: P[BigInt] =
    P(nonzerodigit ~~ digit.rep | "0").!.map(scala.BigInt(_))
  def octinteger[$: P]: P[BigInt] = P(
    "0" ~ ("o" | "O") ~~ octdigit.rep(1).! | "0" ~ octdigit.rep(1).!,
  ).map(scala.BigInt(_, 8))
  def hexinteger[$: P]: P[BigInt] =
    P("0" ~ ("x" | "X") ~~ hexdigit.rep(1).!).map(scala.BigInt(_, 16))
  def bininteger[$: P]: P[BigInt] =
    P("0" ~ ("b" | "B") ~~ bindigit.rep(1).!).map(scala.BigInt(_, 2))
  def nonzerodigit[$: P]: P[Unit] = P(CharIn("1-9"))
  def octdigit[$: P]: P[Unit] = P(CharIn("0-7"))
  def bindigit[$: P]: P[Unit] = P("0" | "1")
  def hexdigit[$: P]: P[Unit] = P(digit | CharIn("a-f", "A-F"))

  def float[$: P]: P[BigDecimal] = P(pointfloat | exponentfloat)
  def pointfloat[$: P]: P[BigDecimal] =
    P(intpart.? ~~ fraction | intpart ~~ "." ~~ !".").!.map(BigDecimal(_))
  def exponentfloat[$: P]: P[BigDecimal] =
    P((intpart | pointfloat) ~~ exponent).!.map(BigDecimal(_))
  def intpart[$: P]: P[BigDecimal] = P(digit.rep(1)).!.map(BigDecimal(_))
  def fraction[$: P]: P[Unit] = P("." ~~ digit.rep(1))
  def exponent[$: P]: P[Unit] = P(("e" | "E") ~~ ("+" | "-").? ~ digit.rep(1))

  // Terms
  def term[$: P]: P[Node] = (codeExpr | semiWrap | semi).m
  def termU[$: P] = P(term.map {
    case Semi(Some(x)) => x
    case x             => x
  })
  def semiWrap[$: P]: P[Node] = P(codeExpr.m ~/ semi.?).map((item, isSemi) =>
    if isSemi.isEmpty then item else Semi(Some(item)),
  )
  def semi[$: P] = P(";").map(_ => Semi(None))
  def primaryExpr[$: P] =
    P(ident | parens | literal | braces).m
  def factor[$: P]: P[Node] = P(unary | primaryExpr.flatMapX(factorR))

  // Braces/Args/Params
  def braces[$: P]: P[Node] =
    P("{" ~/ term.rep.map(_.toList) ~ "}").map(body => {
      // check if all terms are cases
      var caseItems = List.empty[Case]
      var anyNotCase = false
      body.foreach {
        case c: Case    => caseItems = caseItems :+ c
        case Semi(None) =>
        case _          => anyNotCase = true
      }
      if anyNotCase || body.isEmpty then Block(body)
      else CaseBlock(caseItems)
    })
  def parens[$: P] = argList.map(ArgsLit.apply)
  def arg[$: P] = P(spread | keyedArg).m
  def spread[$: P]: P[Node] = P(".." ~/ arg).map(UnOp("..", _))
  def keyedArg[$: P] = P((compound ~ (":" ~/ compound).?).map {
    case (lhs, Some(rhs)) => KeyedArg(lhs, rhs)
    case (lhs, None)      => lhs
  })
  def chk[$: P](s: => P[Unit]) = P(s.?.!).map(_.nonEmpty)

  // Expressions
  def literal[$: P] = P(
    numberLit | booleanLit | noneLit | autoLit | stringLit,
  ).m
  def ident[$: P] = id.map(Ident.apply).m
  def loopItem[$: P] = P(word("loop") ~/ braces).map(Loop.apply)
  def importItem[$: P] =
    P(word("import") ~/ termU ~ (word("from") ~/ termU).?)
      .map {
        case (path, None)       => Import(path, None)
        case (dest, Some(path)) => Import(path, Some(dest))
      }
  def forItem[$: P] = P(
    word("for") ~ "(" ~/ ident ~ word("in") ~ term ~ ")" ~ braces,
  ).map(For.apply.tupled)
  def whileItem[$: P] = P(
    word("while") ~/ parens ~ braces,
  ).map(While.apply)
  def breakItem[$: P] = P(word("break")).map(_ => Break())
  def continueItem[$: P] = P(word("continue")).map(_ => Continue())
  def ifItem[$: P]: P[Node] = P(
    word("if") ~/ parens ~ braces
      ~ (word("else") ~ P(ifItem | braces)).?,
  ).map(If.apply.tupled)
  def returnItem[$: P] = P(word("return") ~/ termU).map(Return.apply)
  // (Top) Compound expressions
  def compound[$: P] = andOr
  def moreAssigns[$: P] =
    "+=" | "-=" | "*=" | "/="
  def assign[$: P]: P[Node] = binOp(P(P("=" ~ !">") | moreAssigns), compound)
  def binOp[$: P](op: => P[Unit], next: => P[Node]): P[Node] =
    P(next ~ (op.! ~/ next).rep).map { case (lhs, rhs) =>
      rhs.foldLeft(lhs) { case (lhs, (op, rhs)) => BinOp(op, lhs, rhs) }
    }
  def andOr[$: P]: P[Node] = binOp(P("and" | "or"), compare)
  def relOp[$: P] = "<=" | ">=" | "==" | "!=" | P("<" ~ !"<") | P(">" ~ !">")
  def inNotIn[$: P] = word("in") | (word("not") ~ word("in"))
  def compare[$: P]: P[Node] = binOp(P(relOp | inNotIn), addSub)
  def addSub[$: P]: P[Node] = binOp(CharIn("+\\-") ~~ !"=", divMul)
  def divMul[$: P]: P[Node] = binOp(CharIn("*/") ~~ !"=", factor)
  def unaryOps[$: P] = "!" | "~" | "-" | "+" | "&" | "*" | word("mut")
  def unary[$: P]: P[Node] = P(unaryOps.! ~ factor).map(UnOp.apply.tupled)
  def eBinR[$: P](e: Node) = select(e) | lambda(e)
  def factorR[$: P](e: Node): P[Node] =
    P(("" ~ eBinR(e)).flatMapX(factorR) | P("").map(_ => e))
  def select[$: P](lhs: Node) =
    P(("." | "::").! ~ ident).map((op, rhs) => Select(lhs, rhs, op != "."))
  def lambda[$: P](lhs: Node) = P("=>" ~/ compound).map(rhs => Lambda(lhs, rhs))
  def initExpression[$: P] = P("=" ~/ term).m

  // Weak Keywords: from
  // Strong Keywords
  val keywords =
    Set(
      // to reduce js load time a bit
      "import|include|let|show|none|auto|set|break|continue|return|case|type|if|else|for|while|and|or|in|not|true|false"
        .split('|')*,
    )
}

private val unescapeStrPattern = "(\\\\[\\\\tfrn\\\"])".r
def unescapeStr(s: String): String = {
  unescapeStrPattern.replaceAllIn(
    s,
    (m) => {
      m.group(1) match {
        case "\\\"" => "\""
        case "\\\\" => "\\"
        case "\\n"  => "\n"
        case "\\t"  => "\t"
        case "\\r"  => "\r"
        case "\\f"  => "\f"
        case other  => other
      }
    },
  )
}
