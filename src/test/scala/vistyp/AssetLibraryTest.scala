package vistyp

class AssetLibraryTest extends munit.FunSuite:
  test("parse package metadata from typst.toml package section") {
    val parsed = PackageTomlParser.parse("""
[package]
name = "basic-shapes"
version = "0.1.0"
entrypoint = "lib.typ"
authors = ["Vistyp"]
""")

    assertEquals(parsed.name, "basic-shapes")
    assertEquals(parsed.version, "0.1.0")
    assertEquals(parsed.entrypoint, "lib.typ")
  }

  test("scan x-prefixed resources with default editable parameters") {
    val key = AssetPackageKey("vistyp", "basic-shapes", "0.1.0")
    val resources = AssetResourceScanner.scan(
      key,
      """
#let helper(value) = value

#let x-server(
  width: 48,
  label: "",
  fill: rgb("#e8eef7"),
  node-label: "",
) = {
  import cetz.draw: *
  rect((0, 0), (width, width), name: node-label, fill: fill)
}
""",
      "basic-shapes@0.1.0",
    )

    assertEquals(resources.map(_.id), List("@vistyp/basic-shapes:0.1.0/x-server"))
    assertEquals(resources.head.args.map(_.name), List("width", "label", "fill"))
    assertEquals(resources.head.args.map(_.kind), List("number", "text", "color"))
  }

  test("skip resources without node-label") {
    val key = AssetPackageKey("vistyp", "basic-shapes", "0.1.0")
    val resources = AssetResourceScanner.scan(
      key,
      """#let x-broken(width: 48) = width""",
      "basic-shapes@0.1.0",
    )

    assertEquals(resources, Nil)
  }
end AssetLibraryTest
