#import "@preview/cetz:0.5.2"

#let flow-label(label) = if type(label) == str {
  label
} else {
  label
}

#let x-flow-start(
  width: 120,
  height: 48,
  label: "Start",
  fill: rgb("#e8f5ee"),
  stroke: rgb("#1f6f43"),
  node-label: "",
) = {
  import cetz.draw: rect, content

  rect(
    (-width / 2, -height / 2),
    (width / 2, height / 2),
    name: node-label,
    fill: fill,
    stroke: stroke,
    radius: 16pt,
  )
  content(node-label, flow-label(label))
}

#let x-flow-process(
  width: 140,
  height: 56,
  label: "Process",
  fill: rgb("#eef4ff"),
  stroke: rgb("#2f5f9f"),
  node-label: "",
) = {
  import cetz.draw: rect, content

  rect(
    (-width / 2, -height / 2),
    (width / 2, height / 2),
    name: node-label,
    fill: fill,
    stroke: stroke,
    radius: 2pt,
  )
  content(node-label, flow-label(label))
}

#let x-flow-decision(
  width: 150,
  height: 86,
  label: "Decision?",
  fill: rgb("#fff7d6"),
  stroke: rgb("#8a6418"),
  node-label: "",
) = {
  import cetz.draw: rect, line, content

  rect(
    (-width / 2, -height / 2),
    (width / 2, height / 2),
    name: node-label,
    fill: none,
    stroke: none,
  )
  line(
    (0, -height / 2),
    (width / 2, 0),
    (0, height / 2),
    (-width / 2, 0),
    close: true,
    name: node-label + "-shape",
    fill: fill,
    stroke: stroke,
  )
  content(node-label, flow-label(label))
}

#let x-flow-io(
  width: 140,
  height: 56,
  skew: 22,
  label: "Input / Output",
  fill: rgb("#f4edff"),
  stroke: rgb("#6a4aa0"),
  node-label: "",
) = {
  import cetz.draw: rect, line, content

  rect(
    (-width / 2, -height / 2),
    (width / 2, height / 2),
    name: node-label,
    fill: none,
    stroke: none,
  )
  line(
    (-width / 2 + skew, -height / 2),
    (width / 2, -height / 2),
    (width / 2 - skew, height / 2),
    (-width / 2, height / 2),
    close: true,
    name: node-label + "-shape",
    fill: fill,
    stroke: stroke,
  )
  content(node-label, flow-label(label))
}

#let x-flow-connector(
  radius: 18,
  label: "",
  fill: rgb("#ffffff"),
  stroke: rgb("#52606d"),
  node-label: "",
) = {
  import cetz.draw: circle, content

  circle(
    (0, 0),
    radius: radius,
    name: node-label,
    fill: fill,
    stroke: stroke,
  )
  content(node-label, flow-label(label))
}

#let x-flow-arrow(
  start: "A.south",
  end: "B.north",
  label: "",
  arrow-mark: (end: ">"),
  label-offset: (0, 0),
  node-label: "",
) = {
  import cetz.draw: line, content, set-style
  let edge-name = if node-label == "" {
    "flow-edge"
  } else {
    node-label
  }

  set-style(mark: (fill: none, size: 12))
  line(start, end, name: edge-name, mark: arrow-mark)
  if label != "" {
    content(
      (rel: label-offset, to: edge-name + ".centroid"),
      flow-label(label),
      fill: white,
      stroke: none,
      padding: 2pt,
    )
  }
}
