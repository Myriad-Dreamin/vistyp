package vistyp

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.annotation.newMain

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import vistyp.syntax.*
import binding.Typst

@main
def bootstrapVistyp(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"), {
      Typst.configureWasm()
      dom.window.asInstanceOf[js.Dynamic].tokyoNightTheme =
        () => tokyoNightTheme()
      dom.window.asInstanceOf[js.Dynamic].onLoadedMonaco =
        (monaco: Monaco) => monacoLoadVar.update(_ => Some(monaco))

      dom.window.asInstanceOf[js.Dynamic].$typst$semanticTokensProvider = Typst
        .getSemanticTokenLegend()
        .`then`(new SemanticTokensProvider(_))

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

  def tagMapping = instances.iterator.zipWithIndex.map {
    case (ins, idx) => {
      val hex = "#" + (idx + 1).formatted("%06x")
      hex -> ins.name
    }
  }.toMap

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
    dom.console.log("constructInstance", node.toString())
    val (nodeName, nodePosRaw, ty, extraArgsRaw) = node match {
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

    val extraArgs = extraArgsRaw match {
      case ArgsLit(args) => Some(args)
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
      case UnOp("+", lhs)  => getNumber(lhs)
      case UnOp("-", lhs)  => -getNumber(lhs)
      case _ => {
        dom.console.error("Invalid number", node.toString())
        0
      }
    }
  }

  // val previewStateVar = Var(PreviewState(defMap, instances))
  // val previewStateSignal = previewStateVar.signal

  val previewContentVar = Var(("<svg></svg>", Map[String, String]()))
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
    val mapping = tagMapping

    var initPos: (Double, Double) = (0, 0)

    instances.iterator.zipWithIndex
      .map {
        case (ins, idx) => {
          val hex = (idx + 1).formatted("%06x")
          val args = ins.extraArgs.iterator.flatten.map(_.repr).mkString(", ")
          val deltaPos = (initPos, ins.pos) match {
            case ((x, y), Some((dx, dy))) => {
              initPos = (dx, dy)
              // (x + dx, y - dy)
              (dx - x, dy - y)
            }
            case (initPos, None) => (0, 0)
          }

          s"""
translate($deltaPos)
rect((0, 0), (1, 1), stroke: 0.00012345pt + rgb("#$hex"))
${ins.ty}(${args}, node-label: "${ins.name}")
rect((0, 0), (1, 1), stroke: 0.00012345pt + rgb("#$hex"))
          """
        }
      }
      .mkString("\n")
  }

  def updatePreview() = {
    dom.console.log("update preview", instances.toString())
    // dom.console.log("update preview", defMap.toString(), instances.toString())

    // val state = PreviewState(defMap, instances)

    val width = 600
    val height = 600

    val content = s"""
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
    """
    // println(s"preview main $content")
    Typst
      .previewSvg(content)
      .`then` { svg =>
        // dom.console.log("svgToCheck", js.Dynamic.literal(svg = svg))
        previewContentVar.update(_ => (svg, tagMapping))
      }
  }

  val sampleDiagram = """#{
  make-ins("c0", (0, 0), "x-circle", (rad: 50))
  make-ins("c1", (50, 50), "x-rect", (x: 100))
  make-ins("c2", (200, 0), "x-circle", (rad: 50))
  make-ins("c1toc2", (0, 0), "x-arrow", (start: "c1.center", end: "c2.center"))
}
"""

  val programContentVar = Var(sampleDiagram)
  val programContentSignal = programContentVar.signal

  def updateProgram() = {
    var content = instances
      .map { ins =>
        val args = ins.extraArgs.iterator.flatten.map(_.repr).mkString(", ")
        s"""make-ins("${ins.name}", (${ins.pos.get._1}, ${ins.pos.get._2}), "${ins.ty}", (${args}))"""
      }
      .mkString("#{", "\n  ", "}")

    programContentVar.update(_ => content)
  }

  def onPreviewMounted(panel: dom.Element): Unit = {
    panel.addEventListener("mousedown", e => this.startDrag(panel, e));
    panel.addEventListener("mousemove", e => this.drag(panel, e));

    panel.addEventListener("mouseup", e => this.endDrag(panel, e));
    panel.addEventListener("mouseleave", e => this.endDrag(panel, e));
    // panel.addEventListener(
    //   "contextmenu",
    //   e => this.doToggleContextMenu(panel, e),
    // );
  }

  var selectedElem: Option[dom.Element] = None
  var offset: (Double, Double) = (0, 0)
  var svgElem: Option[dom.SVGElement] = None
  var transform = Option.empty[dom.SVGTransform]
  def startDrag(panel: dom.Element, evt: dom.WheelEvent): Unit = {

    val targetOpt =
      findTaggedTypstElement(evt.target.asInstanceOf[dom.Element]);

    val target = targetOpt match {
      case Some(target) => target
      case None =>
        selectedElem = None
        return
    }
    svgElem = findSvgElement(panel)
    dom.console.log("svgElem", svgElem);

    selectedElem = Some(target)
    offset = getMousePosition(evt)

    val transforms = target
      .asInstanceOf[js.Dynamic]
      .transform
      .baseVal
      .asInstanceOf[js.Array[dom.SVGTransform]]
    if (
      transforms.length == 0 ||
      transforms(0).`type` != dom.SVGTransform.SVG_TRANSFORM_TRANSLATE
    ) {
      val translate = svgElem.get.asInstanceOf[js.Dynamic].createSVGTransform()
      translate.setTranslate(0, 0)
      target
        .asInstanceOf[js.Dynamic]
        .transform
        .baseVal
        .insertItemBefore(
          translate,
          0,
        )
    }

    transform = Some(transforms(0))
    offset =
      (offset._1 - transform.get.matrix.e) -> (offset._2 - transform.get.matrix.f)

    val typstId = getTypstElementId(target)
    println(s"find typstId $typstId in $instances")
    instances = instances.map { ins =>
      if (ins.name == typstId) {
        ins.copy(initPos = ins.pos)
      } else {
        ins
      }
    }
  }

  def drag(panel: dom.Element, evt: dom.WheelEvent): Unit = {
    val selectedElement = selectedElem match {
      case Some(selectedElem) => selectedElem
      case None               => return
    }

    evt.preventDefault()
    val coord = getMousePosition(evt)
    val x = coord._1 - offset._1
    val y = coord._2 - offset._2
    transform.get.setTranslate(x, y)
    val typstId = getTypstElementId(selectedElement)
    println(s"find typstId $typstId in $instances")
    instances = instances.map { ins =>
      if (ins.name == typstId) {
        ins.copy(pos = Some(x -> y))
      } else {
        ins
      }
    }
    // updatePreview()
  }

  def endDrag(panel: dom.Element, evt: dom.WheelEvent): Unit = {
    val selectedElement = selectedElem match {
      case Some(selectedElem) => selectedElem
      case None               => return
    }
    selectedElem = None

    val typstId = getTypstElementId(selectedElement)
    println(s"find typstId $typstId in $instances")
    val ins = instances.find(_.name == typstId).get
    if (ins.pos.isDefined) {
      val x = ins.initPos.get._1 + ins.pos.get._1
      val y = ins.initPos.get._2 - ins.pos.get._2
      instances = instances.map { ins =>
        if (ins.name == typstId) {
          ins.copy(pos = Some(x -> y), initPos = None)
        } else {
          ins
        }
      }

      updateProgram()
    }
  }

  def getTypstElementId(target: dom.Element): String = {
    target.getAttribute("data-element-id").replace("cetz-app-", "")
  }

  /** A Fetch Action from DOM
    */
  def getMousePosition(evt: dom.WheelEvent) = {
    val CTM = svgElem.get
      .asInstanceOf[js.Dynamic]
      .getScreenCTM()
      .asInstanceOf[dom.SVGMatrix]
    (evt.clientX - CTM.e) / CTM.a -> (evt.clientY - CTM.f) / CTM.d
  }

  def findSvgElement(panel: dom.Element): Option[dom.SVGElement] = {
    Some(panel.querySelector("svg").asInstanceOf[dom.SVGElement])
  }

  def findTaggedTypstElement(target: dom.Element): Option[dom.Element] = {
    var current = target
    while (current != null) {
      dom.console.log("current", current)
      if (
        current.classList != null && current.classList.contains(
          "typst-cetz-elem",
        )
      ) {
        return Some(current)
      }
      current =
        current.asInstanceOf[js.Dynamic].parentElement.asInstanceOf[dom.Element]
    }
    None
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
    val extraArgs: Option[List[syntax.Node]],
    val initPos: Option[(Double, Double)] = None,
)
