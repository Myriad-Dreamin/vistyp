package vistyp

import vistyp.syntax.*

object DiagramParser:
  def statements(code: String): Option[List[Node]] =
    statements(parseCode(code))

  def statements(program: MarkupBlock): Option[List[Node]] =
    program.stmts.filterNot(isBlankMarkup) match
      case List(Block(stmts)) => Some(stmts.filterNot(isBlankMarkup))
      case _                  => None

  private def isBlankMarkup(node: Node): Boolean =
    node match
      case MarkupContent(value) => value.trim.isEmpty
      case _                    => false
