package vistyp

class GridTest extends munit.FunSuite:
  test("disabled grid keeps coordinates free") {
    val settings = GridSettings(enabled = false, size = 10)
    assertEquals(Grid.snapPoint(12.3456 -> -7.8912, settings), 12.346 -> -7.891)
  }

  test("enabled grid snaps to nearest grid line") {
    val settings = GridSettings(enabled = true, size = 10)
    assertEquals(Grid.snapPoint(14d -> -16d, settings), 10d -> -20d)
  }

  test("invalid grid sizes are treated as inactive") {
    val settings = GridSettings(enabled = true, size = 0)
    assertEquals(Grid.snap(12.3456, settings), 12.346)
  }

  test("offset snap aligns a visual reference point") {
    val settings = GridSettings(enabled = true, size = 20)
    assertEquals(
      Grid.snapPointWithOffset(0d -> 0d, -50d -> -50d, settings),
      10d -> 10d,
    )
  }
end GridTest
