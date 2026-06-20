#import "@preview/cetz:0.5.2"

#let x-note(
  width: 120,
  height: 72,
  label: "Note",
  fill: rgb("#f7f2db"),
  stroke: rgb("#384250"),
  node-label: "",
) = {
  import cetz.draw: rect, content

  rect(
    (0, 0),
    (width, height),
    name: node-label,
    fill: fill,
    stroke: stroke,
    radius: 4pt,
  )
  content(node-label, label)
}
