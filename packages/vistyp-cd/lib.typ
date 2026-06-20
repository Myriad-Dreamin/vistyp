#import "@preview/cetz:0.5.2"

#let math-label(label) = if type(label) == str {
  eval(label, mode: "math")
} else {
  label
}

#let x-cd-object(
  width: 64,
  height: 36,
  label: "A",
  fill: none,
  stroke: none,
  node-label: "",
) = {
  import cetz.draw: rect, content

  rect(
    (-width / 2, -height / 2),
    (width / 2, height / 2),
    name: node-label,
    fill: fill,
    stroke: stroke,
  )
  content(node-label, math-label(label))
}

#let x-cd-arrow(
  start: "A.east",
  end: "B.west",
  label: "f",
  arrow-mark: (end: ">"),
  label-offset: (0, 12),
  node-label: "",
) = {
  import cetz.draw: line, content, set-style
  let morphism-name = if node-label == "" {
    "morphism"
  } else {
    node-label
  }

  set-style(mark: (fill: none, size: 14))
  line(start, end, name: morphism-name, mark: arrow-mark)
  content(
    (rel: label-offset, to: morphism-name + ".centroid"),
    math-label(label),
    fill: white,
    stroke: none,
    padding: 2pt,
  )
}

#let x-cd-2cell(
  start: "f.centroid",
  end: "g.centroid",
  label: "",
  arrow-mark: (end: ">"),
  label-offset: (0, 10),
  node-label: "",
) = {
  import cetz.draw: line, content, set-style
  let cell-name = if node-label == "" {
    "cell"
  } else {
    node-label
  }

  set-style(mark: (fill: none, size: 10))
  line(start, end, name: cell-name, mark: arrow-mark)
  content(
    (rel: label-offset, to: cell-name + ".centroid"),
    math-label(label),
    fill: white,
    stroke: none,
    padding: 2pt,
  )
}
