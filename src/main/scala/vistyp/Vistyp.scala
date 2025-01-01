package vistyp

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}
import vistyp.syntax.*
import scala.annotation.newMain

@main
def bootstrapVistyp(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"), {
      Typst.configureWasm()
      dom.console.log("TypstTs", TypstTs)
      dom.window.asInstanceOf[js.Dynamic].$typst = TypstTs
      dom.window.asInstanceOf[js.Dynamic].tokyoNightTheme =
        () => tokyoNightTheme()
      dom.window.asInstanceOf[js.Dynamic].onLoadedMonaco =
        (monaco: Monaco) => monacoLoadVar.update(_ => Some(monaco))

      dom.window.asInstanceOf[js.Dynamic].$typst$semanticTokensProvider =
        TypstTs
          .getSemanticTokenLegend()
          .`then`(legend => {
            dom.console.log("legend", legend)
            new SemanticTokensProvider(legend)
          })

      UI(VistypImpl).mainElement()
    },
  )
end bootstrapVistyp

// case class PreviewState(
//     val defMap: Map[String, Def],
//     val instances: List[Instance],
// )

object VistypImpl extends Vistyp:
  var defMap: Map[String, Def] = Map.empty
  var instances: List[Instance] = List()

  def updateDefinition(code: String): Unit = {
    this.loadDefinition(parseCode(code))
  }

  def loadDefinition(program: syntax.MarkupBlock) = {
    val newDefMap = program.stmts.collect {
      case syntax.LetBinding(name, param, init) =>
        name.name -> Def(name.offset, name.name, param, init)
    }.toMap

    defMap = newDefMap
    updatePreview()
  }

  def updateDiagram(code: String): Unit = {
    val program = parseCode(code)
    dom.console.log("diagram program", program.toString())
    val newInstances = program match {
      case syntax.MarkupBlock(List(syntax.Block(stmts))) =>
        stmts.flatMap(constructInstance)
      case program =>
        dom.console.error("Invalid diagram program", program)
        List()
    }

    instances = newInstances
    updatePreview()
  }

  def constructInstance(node: syntax.Node): Option[Instance] = {
    val (nodeName, nodePosRaw, ty, extraArgs) = node match {
      case Apply(
            Ident("make-ins"),
            List(StrLit(nodeName), nodePos, StrLit(ty), extraArgs),
          ) =>
        (nodeName, nodePos, ty, extraArgs)
      case _ => {
        return None
      }
    }

    val nodePos = nodePosRaw match {
      case ArgsLit(List(x, y)) => Some((getNumber(x), getNumber(y)))
      case _ => {
        dom.console.error("Invalid node position", nodePosRaw)
        None
      }
    }

    Some(Instance(nodeName, nodePos, ty, extraArgs))
  }

  def getNumber(node: syntax.Node): Double = {
    node match {
      case IntLit(value)   => value.toDouble
      case FloatLit(value) => value.toDouble
      case _ => {
        dom.console.error("Invalid number", node)
        0
      }
    }
  }

  // val previewStateVar = Var(PreviewState(defMap, instances))
  // val previewStateSignal = previewStateVar.signal

  val previewContentVar = Var("<svg></svg>")
  val previewContentSignal = previewContentVar.signal

  def genSignatures(): String = {

    """
let x-circle(rad: 200, inner-text: "", node-label: "") = {
  circle((0, 0), radius: rad, name: node-label)
  debug-label((-rad*0.7, -rad*0.7))
  // content(node-label, inner-text)
}
let x-rect(x: 200, y: none, inner-text: "", node-label: "") = {
  let y = if y == none {
    x
  } else {
    y
  }
  rect((0, 0), (x, y), name: node-label)
  debug-label((0, 0))
  // content(node-label, inner-text)
}
let x-arrow(start: (0, 10), end: (50, 10), inner-text: "", mark: (end: ">"), node-label: "") = {
  set-style(mark: (fill: none, size: 14))
  line(start, end, name: "t", mark: mark)
  content("t.centroid", inner-text)
}
    """
  }

  def genInstances(): String = {

    """
translate((0, 0))
rect((0, 0), (1, 1), stroke: 0.00012345pt + rgb("#000001"))
x-circle(rad: 50, node-label: "c0")
rect((0, 0), (1, 1), stroke: 0.00012345pt + rgb("#000001"))
translate((50, 50))
rect((0, 0), (1, 1), stroke: 0.00012345pt + rgb("#000002"))
x-rect(x: 50, node-label: "c1")
rect((0, 0), (1, 1), stroke: 0.00012345pt + rgb("#000002"))
translate((150, -50))
rect((0, 0), (1, 1), stroke: 0.00012345pt + rgb("#000003"))
x-circle(rad: 50, node-label: "c2")
rect((0, 0), (1, 1), stroke: 0.00012345pt + rgb("#000003"))
translate((-200, 0))
x-arrow(start: "c1.center", end: "c2.center", node-label: "c1toc2")
// make-ins("c0", (0, 0), "x-circle", (rad: 50))
// make-ins("c1", (50, 50), "x-rect", (x: 100))
// make-ins("c2", (200, 0), "x-circle", (rad: 50))
// make-ins("c1toc2", (0, 0), "x-arrow", (start: "c1", end: "c2"))
"""
  }

  def updatePreview() = {
    dom.console.log("update preview", defMap.toString(), instances.toString())

    // val state = PreviewState(defMap, instances)

    val width = 600
    val height = 600

    Typst
      .previewSvg(s"""
#import "@preview/cetz:0.3.1"
// #place(dx: 0pt, dy: 0pt, circle())
// #place(dx: 50pt, dy: 50pt, square())
// #place(dx: 200pt, dy: 0pt, circle())
// #place(dx: 50pt, dy: 50pt, square())

// #set page(margin: 1pt, width: ${width + 2}pt, height: ${height + 2}pt)
#let debug-label(_) = ()
#cetz.canvas({
  import cetz.draw: *
  let node-label = ""
  ${genSignatures()}
  ${genInstances()}
}, length: 1pt)
""").`then` { svg =>
      dom.console.log("svgToCheck", js.Dynamic.literal(svg = svg))
      previewContentVar.update(_ => svg)
    }
  }

end VistypImpl

case class Def(
    val offset: Int,
    val name: String,
    val param: Option[List[syntax.Node]],
    val init: syntax.Node,
)

case class Instance(
    val name: String,
    val pos: Option[(Double, Double)],
    val ty: String,
    val extraArgs: syntax.Node,
)
