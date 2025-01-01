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
    P((!End ~ markupExpr).rep.map(_.toList) ~ End).map(MarkupBlock.apply).m
  def markupMode[$: P]: P[MarkupBlock] =
    P((!End ~ markupExpr).rep.map(_.toList)).map(MarkupBlock.apply).m
  def codeMode[$: P]: P[Block] =
    P(("" ~ codeExpr).rep.map(_.toList)).map(Block.apply).m
  def mathMode[$: P]: P[MathBlock] =
    P(("" ~ mathExpr).rep.map(_.toList)).map(MathBlock.apply).m

  def markupExpr[$: P]: P[Node] = P(
    hashExpr | equation | blockRaw | inlineRaw | regularMarkup,
  )
  def lazyCodeExpr[$: P]: P[Node] = P(
    lazyCodeBlock | lazyContentBlock | codeExpr,
  )
  def codeExpr[$: P]: P[Node] = P(atomicExprNoPrimary | assign)
  def atomicExprNoPrimary[$: P]: P[Node] = P(
    letBindingItem | ifItem | forItem | whileItem | breakItem | continueItem | returnItem | showItem | setItem | importItem | includeItem,
  )
  def mathExpr[$: P]: P[Node] = P(
    hashExpr | escapeseq.!.map(EscapeLit.apply) | regularMarkup,
  )

  def codeBlock[$: P]: P[Node] = P(
    "{" ~/ codeMode ~ "}",
  )
  def contentBlock[$: P]: P[Node] = P(
    "[" ~/ markupMode ~ "]",
  )
  def equation[$: P]: P[Node] = P(
    "$" ~/ mathMode ~ "$",
  )

  def hashExpr[$: P]: P[Node] = P(
    "#" ~ (atomicExprNoPrimary | primaryExpr),
  )

  def lazyCodeBlock[$: P]: P[Node] = codeBlock
  def lazyContentBlock[$: P]: P[Node] = contentBlock

  // Markup

  def regularMarkup[$: P]: P[Node] =
    P(regularMarkupContent.repX.!).map(MarkupContent.apply)
  def regularMarkupContent[$: P]: P[Unit] = P(
    ("[" ~~ regularMarkupContent ~~ ("]" | End)) |
      !(End | "#" | "$" | "`" | "]") ~~/ (fastSkipMarkup | escapeseq),
  )

  def fastSkipMarkup[$: P] = CharsWhile(!"#[]$`\\".contains(_))
  def blockRaw[$: P]: P[Node] = P(
    "`"
      .rep(min = 3)
      .!
      .flatMapX(delim => rawBody(delim).! ~~ (delim | End))
      .map(RawContent.apply),
  )
  def inlineRaw[$: P]: P[Node] =
    P("``".map(_ => "") | "`" ~/ rawBody("`").! ~ "`").map(RawContent.apply)
  def rawBody[$: P](delim: String) = P(
    (!(delim | End) ~/ CharsWhile(!"`".contains(_))).rep.!,
  )

  // Literals
  def whitespaceInline[$: P] = P(CharsWhileIn(" \t").rep)
  def whitespace[$: P] = P(CharsWhileIn(" \t\n").rep)
  def noneLit[$: P] = P("none").map(_ => NoneLit)
  def autoLit[$: P] = P("auto").map(_ => AutoLit)
  def booleanLit[$: P] =
    (P(word("true") | word("false"))).!.map(v => BoolLit(v == "true"))
  def numberLit[$: P]: P[FloatLit | IntLit] = P(
    float.map(FloatLit.apply) | int.map(IntLit.apply),
  )
  def numberOrLengthLit[$: P] = P(
    P(numberLit ~~ ident.?).map {
      case (value, None)       => value
      case (value, Some(unit)) => LengthLit(value, unit.name)
    },
  )
  def stringLit[$: P] = P(longStr | shortStr).map(StrLit.apply)
  def labelLit[$: P] =
    P("<" ~~/ (idCont | ":" | ".").repX.! ~~ ">").map(LabelLit.apply)

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
    P(ident | parens | literal | codeBlock | contentBlock).m
  def factor[$: P]: P[Node] = P(unary | primaryExpr.flatMapX(factorR))

  // Braces/Args/Params
  def parens[$: P] = argList.map(ArgsLit.apply)
  def paramList[$: P]: P[List[Node]] = P(
    "(" ~/ (ident ~ defaultValue.?).map(Param.apply.tupled).rep(sep = ",") ~ ")",
  ).map(_.toList)
  def defaultValue[$: P] = P(":" ~/ codeExpr).m
  def argList[$: P]: P[List[Node]] = P(
    "(" ~/ (namedArg | spreadArg | codeExpr).rep(sep = ",") ~ ")",
  ).map(_.toList)
  def namedArg[$: P]: P[Node] = P(
    ident ~ ":" ~/ codeExpr,
  ).map(KeyedArg.apply.tupled)

  def spreadArg[$: P]: P[Node] = P(".." ~/ codeExpr).map(UnOp("..", _))
  def chk[$: P](s: => P[Unit]) = P(s.?.!).map(_.nonEmpty)

  // Expressions
  def literal[$: P] = P(
    numberOrLengthLit | booleanLit | noneLit | autoLit | stringLit | labelLit,
  ).m
  def ident[$: P] = id.map(Ident.apply).m
  def anyBlock[$: P]: P[Node] = P(
    codeBlock | contentBlock,
  )
  def ifItem[$: P]: P[Node] = P(
    "if" ~/ codeExpr ~ anyBlock ~ ("else" ~ (anyBlock | ifItem)).?,
  ).map(If.apply.tupled)
  def forItem[$: P] = P(
    word("for") ~ "(" ~/ ident ~ word("in") ~ term ~ ")" ~ anyBlock,
  ).map(For.apply.tupled)
  def whileItem[$: P] = P(
    word("while") ~/ parens ~ anyBlock,
  ).map(While.apply)
  def breakItem[$: P] = P(word("break")).map(_ => Break())
  def continueItem[$: P] = P(word("continue")).map(_ => Continue())
  def returnItem[$: P] = P(word("return") ~/ termU).map(Return.apply)
  def applyItem[$: P]: P[Node] = P(ident ~ argList).map(Apply.apply.tupled)
  def letBindingItem[$: P]: P[Node] = P(
    "let" ~/ ident ~/ paramList.? ~ "=" ~/ lazyCodeExpr,
  ).map(LetBinding.apply.tupled)
  def importItem[$: P] =
    P(
      word("import") ~/ codeExpr ~ (word(
        "as",
      ) ~/ ident).? ~ (":" ~/ (importList(false) | "(" ~/ importList(
        true,
      ) ~ ")")).?,
    )
      .map(Import.apply.tupled)
  def includeItem[$: P] = P(word("include") ~/ codeExpr).map(IncludeItem.apply)
  def importList[$: P](allowNewLine: Boolean): P[List[Node]] = P(
    if allowNewLine then {
      importPath.rep(sep = ",").map(_.toList)
    } else {
      importPath.repX(sep = P("" ~~ whitespaceInline ~~ ",")).map(_.toList)
    },
  )
  def showItem[$: P] =
    P(
      word("show") ~/ codeExpr.? ~ (":" ~/ codeExpr.?).?.map(_.flatten),
    ).map(Show.apply.tupled)
  def setItem[$: P] = P(
    word("set") ~/ codeExpr ~ ("if" ~ codeExpr).?,
  ).map(SetItem.apply.tupled)

  def importPath[$: P]: P[Node] = ident
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
  def unaryOps[$: P] = "!" | "~" | "-" | "+" | "&" | "*" | word("context")
  def unary[$: P]: P[Node] = P(unaryOps.! ~ factor).map(UnOp.apply.tupled)
  def eBinR[$: P](e: Node) = select(e) | lambda(e)
  def factorR[$: P](e: Node): P[Node] =
    P(("" ~ eBinR(e)).flatMapX(factorR) | P("").map(_ => e))
  def select[$: P](lhs: Node) =
    P((".") ~ ident).map((rhs) => Select(lhs, rhs))
  def lambda[$: P](lhs: Node) = P("=>" ~/ compound).map(rhs => Lambda(lhs, rhs))

  // Weak Keywords: from
  // Strong Keywords
  val keywords =
    Set(
      // to reduce js load time a bit
      "import|include|let|show|none|auto|set|break|continue|return|case|type|if|else|for|while|and|or|in|not|true|false|context"
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
