#{
  make-ins("start", (0, 0), "@vistyp/vistyp-flowchart:0.1.0/x-flow-start", (label: "Start"))
  make-ins("read", (0, -92), "@vistyp/vistyp-flowchart:0.1.0/x-flow-io", (label: "Read n"))
  make-ins("check", (0, -196), "@vistyp/vistyp-flowchart:0.1.0/x-flow-decision", (label: "n <= 1?"))
  make-ins("return-base", (-170, -318), "@vistyp/vistyp-flowchart:0.1.0/x-flow-process", (label: "Return 1"))
  make-ins("multiply", (170, -318), "@vistyp/vistyp-flowchart:0.1.0/x-flow-process", (label: "n * fact(n - 1)"))
  make-ins("end", (0, -430), "@vistyp/vistyp-flowchart:0.1.0/x-flow-start", (label: "End"))
  make-ins("a-start-read", (0, 0), "@vistyp/vistyp-flowchart:0.1.0/x-flow-arrow", (start: "start.south", end: "read.north"))
  make-ins("a-read-check", (0, 0), "@vistyp/vistyp-flowchart:0.1.0/x-flow-arrow", (start: "read.south", end: "check.north"))
  make-ins("a-check-base", (0, 0), "@vistyp/vistyp-flowchart:0.1.0/x-flow-arrow", (start: "check.west", end: "return-base.north", label: "yes", label-offset: (-18, 0)))
  make-ins("a-check-mul", (0, 0), "@vistyp/vistyp-flowchart:0.1.0/x-flow-arrow", (start: "check.east", end: "multiply.north", label: "no", label-offset: (18, 0)))
  make-ins("a-base-end", (0, 0), "@vistyp/vistyp-flowchart:0.1.0/x-flow-arrow", (start: "return-base.south", end: "end.west"))
  make-ins("a-mul-end", (0, 0), "@vistyp/vistyp-flowchart:0.1.0/x-flow-arrow", (start: "multiply.south", end: "end.east"))
}
