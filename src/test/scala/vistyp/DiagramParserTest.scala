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

  test("extract formula triangle diagram statements") {
    val code = """#{
  make-ins("A", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-object", (label: "pi_1(X,x)"))
  make-ins("p", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-arrow", (start: "A.south-west", end: "B.north-east", label: "p_*", label-offset: (-12, 10)))
}
"""

    val statements = DiagramParser.statements(code).getOrElse(Nil)
    assertEquals(statements.length, 2)
    assertEquals(
      statements(1).repr,
      """make-ins("p", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-arrow", (start: "A.south-west", end: "B.north-east", label: "p_*", label-offset: (-12, 10)))""",
    )
  }
end DiagramParserTest
