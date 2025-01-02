package vistyp.syntax

import vistyp.escapeStr

import fastparse._

private type N = Ident
private type Str = String
private type Pol = Option[List[Param]]
private[vistyp] type No = Option[Node]
type TmplExp = Option[(Node, Option[String])]

sealed abstract class Node {
  var offset: Int = -1;
  var end: Int = -1;

  def repr: String = {
    this match {
      case KeyedArg(key, value) => s"${key.repr}: ${value.repr}"
      case Ident(name)          => name
      case StrLit(value)        => s""""${escapeStr(value)}""""
      case IntLit(value)        => value.toString
      case FloatLit(value)      => value.toString
    }
  }
}

object NodeParse {
  implicit class Mapper[T <: Node](n: => P[T])(implicit ctx: P[_]) {
    def m = {
      val l = ctx.index;
      val r = n.map(node => { node.offset = l; node.end = ctx.index; node });
      ctx.asInstanceOf[P[T]]
    }
  }
}

// Kind: Decorators
// A node that is terminated by a semicolon
final case class Semi(semi: No) extends Node
// A node that is decorated by another node
final case class Decorate(lhs: Node, rhs: Node) extends Node

// Kind: Literals
final case class Err(msg: String) extends Node
// Just panic on problematic impls
object TodoLit extends Node
// Identifier
final case class Ident(name: Str) extends Node
// Boolean Literal
final case class BoolLit(value: Boolean) extends Node
// None Literal
case object NoneLit extends Node
// auto Literal
case object AutoLit extends Node
// Integer Literal
final case class IntLit(value: BigInt) extends Node
// Float Literal
final case class FloatLit(value: BigDecimal) extends Node
final case class LengthLit(value: IntLit | FloatLit, unit: Str) extends Node
final case class MarkupContent(value: Str) extends Node
final case class RawContent(value: Str) extends Node
final case class StrLit(value: Str) extends Node
final case class LabelLit(value: Str) extends Node
final case class EscapeLit(value: Str) extends Node
// todo: Param Literal, remove me
final case class ParamsLit(values: List[Param]) extends Node
// Argument Literal (Named and Nameless Tuples)
final case class ArgsLit(values: List[Node]) extends Node

// Kind: Blocks
// A block of statements
final case class MarkupBlock(stmts: List[Node]) extends Node
final case class MathBlock(stmts: List[Node]) extends Node
final case class Block(stmts: List[Node]) extends Node
// Kind: Var Decls
// constant level-0 variable
final case class LetBinding(name: N, param: Option[List[Node]], init: Node)
    extends Node
final case class Param(name: N, init: No) extends Node
final case class Apply(lhs: Node, rhs: List[Node]) extends Node
final case class SetItem(rule: Node, cond: No) extends Node
final case class Show(selector: No, transform: No) extends Node
final case class Import(path: Node, new_name: No, items: Option[List[Node]])
    extends Node
final case class IncludeItem(path: Node) extends Node
// Kind: Control Flow
final case class While(cond: Node, body: Node) extends Node
final case class For(name: N, iter: Node, body: Node) extends Node
final case class If(cond: Node, cont_bb: Node, else_bb: No) extends Node
final case class Break() extends Node
final case class Continue() extends Node
final case class Return(value: Node) extends Node
// Kind: Expressions
final case class UnOp(op: Str, lhs: Node) extends Node
final case class BinOp(op: Str, lhs: Node, rhs: Node) extends Node
final case class Select(lhs: Node, rhs: Ident) extends Node
final case class Lambda(lhs: Node, rhs: Node) extends Node
final case class KeyedArg(key: Node, value: Node) extends Node
// Kind: Clauses
