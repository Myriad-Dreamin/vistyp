#import "@preview/cetz:0.5.2"

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
  let morphism-name = if node-label == "" {
    "morphism"
  } else {
    node-label
  }

  set-style(mark: (fill: none, size: 14))
  line(start, end, name: morphism-name, mark: mark)
  content(morphism-name + ".centroid", label)
}

#let x-cd-2cell(
  start: "f.centroid",
  end: "g.centroid",
  label: "",
  mark: (end: ">"),
  node-label: "",
) = {
  import cetz.draw: *
  let cell-name = if node-label == "" {
    "cell"
  } else {
    node-label
  }

  set-style(mark: (fill: none, size: 10))
  line(start, end, name: cell-name, mark: mark)
  content(cell-name + ".centroid", label)
}
