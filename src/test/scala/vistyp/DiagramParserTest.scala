package vistyp

class DiagramParserTest extends munit.FunSuite:
  test("extract diagram statements with trailing whitespace markup") {
    val code = """#{
  make-ins("A", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-object", (label: "A", fill: none))
}
"""

    val statements = DiagramParser.statements(code)
    assertEquals(statements.map(_.length), Some(1))
  }

  test("repr supports defaults emitted by resource insertion") {
    val code = """#{
  make-ins("A", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-object", (label: "A", fill: none, stroke: rgb("#384250")))
}
"""
    val args = DiagramParser
      .statements(code)
      .toList
      .flatten
      .collectFirst {
        case syntax.Apply(
              syntax.Ident("make-ins"),
              List(_, _, _, syntax.ArgsLit(args)),
            ) =>
          args.map(_.repr)
      }

    assertEquals(
      args,
      Some(List("label: \"A\"", "fill: none", "stroke: rgb(\"#384250\")")),
    )
  }
end DiagramParserTest
