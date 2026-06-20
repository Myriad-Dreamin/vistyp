#{
  make-ins("A", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-object", (label: "pi_1(X,x)"))
  make-ins("B", (-160, -110), "@vistyp/vistyp-cd:0.1.0/x-cd-object", (label: "pi_1(Y,p(x))"))
  make-ins("C", (160, -110), "@vistyp/vistyp-cd:0.1.0/x-cd-object", (label: "pi_1(Y,q(x))"))
  make-ins("p", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-arrow", (start: "A.south-west", end: "B.north-east", label: "p_*", label-offset: (-12, 10)))
  make-ins("q", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-arrow", (start: "A.south-east", end: "C.north-west", label: "q_*", label-offset: (12, 10)))
  make-ins("gamma", (0, 0), "@vistyp/vistyp-cd:0.1.0/x-cd-arrow", (start: "B.east", end: "C.west", label: "gamma [a]", label-offset: (0, -14)))
}
