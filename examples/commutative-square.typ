#{
  make-ins("A", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-object", (label: "A"))
  make-ins("B", (180, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-object", (label: "B"))
  make-ins("C", (0, -120), "@vistyp/vistyp-cd:0.1.0/x-cd-object", (label: "C"))
  make-ins("D", (180, -120), "@vistyp/vistyp-cd:0.1.0/x-cd-object", (label: "D"))
  make-ins("f", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-arrow", (start: "A.east", end: "B.west", label: "f"))
  make-ins("g", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-arrow", (start: "A.south", end: "C.north", label: "g"))
  make-ins("h", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-arrow", (start: "B.south", end: "D.north", label: "h"))
  make-ins("k", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-arrow", (start: "C.east", end: "D.west", label: "k"))
}
