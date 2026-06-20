#let x-cd-object(
  width: 64,
  height: 36,
  label: "A",
  fill: none,
  stroke: none,
  node-label: "",
) = {
  import cetz.draw: *

  rect(
    (-width / 2, -height / 2),
    (width / 2, height / 2),
    name: node-label,
    fill: fill,
    stroke: stroke,
  )
  content(node-label, label)
}

#let x-cd-arrow(
  start: "A.east",
  end: "B.west",
  label: "f",
  mark: (end: ">"),
  node-label: "",
) = {
  import cetz.draw: *

  set-style(mark: (fill: none, size: 14))
  line(start, end, name: "morphism", mark: mark)
  content("morphism.centroid", label)
}

#let x-cd-2cell(
  start: "f.centroid",
  end: "g.centroid",
  label: "",
  mark: (end: ">"),
  node-label: "",
) = {
  import cetz.draw: *

  set-style(mark: (fill: none, size: 10))
  line(start, end, name: "cell", mark: mark)
  content("cell.centroid", label)
}
