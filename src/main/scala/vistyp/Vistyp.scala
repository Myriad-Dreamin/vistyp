package vistyp

import scala.scalajs.js
import scala.scalajs.js.annotation.*

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
      val hex = "#" + "%06x".format(idx + 1)
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
    val newInstances = DiagramParser.statements(program) match {
      case Some(stmts) =>
        stmts.flatMap(constructInstance)
      case None =>
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

  def genPackageImports(): (String, Map[AssetPackageKey, String]) = {
    val packageByKey = loadedAssetPackages.map(pkg => pkg.key -> pkg).toMap
    val usedKeys = instances
      .flatMap(ins => ResourceRef.parse(ins.ty).map(_.packageKey))
      .distinct
      .filter(packageByKey.contains)
    val aliases = usedKeys.zipWithIndex.map { case (key, idx) =>
      key -> s"pkg$idx"
    }.toMap
    val imports = aliases
      .map { case (key, alias) =>
        val pkg = packageByKey(key)
        s"""import "${pkg.entrypointVirtualPath}" as $alias"""
      }
      .mkString("\n")

    imports -> aliases
  }

  def genInstances(packageAliases: Map[AssetPackageKey, String]): String = {
    val mapping = tagMapping

    var initPos: (Double, Double) = (0, 0)

    instances.iterator.zipWithIndex
      .map {
        case (ins, idx) => {
          val hex = "%06x".format(idx + 1)
          val args = (ins.extraArgs.iterator.flatten.map(_.repr).toList :+
            s"""node-label: "${escapeStr(ins.name)}"""").mkString(", ")
          val callable = ResourceRef
            .parse(ins.ty)
            .flatMap(ref =>
              packageAliases.get(ref.packageKey).map(alias =>
                s"$alias.${ref.functionName}",
              ),
            )
            .getOrElse(ins.ty)
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
$callable($args)
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
    val (packageImports, packageAliases) = genPackageImports()
    val packageSources = loadedAssetPackages.flatMap(_.files).toMap

    val content = s"""
    #import "@preview/cetz:0.5.2"
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
      $packageImports
      ${genInstances(packageAliases)}
    }, length: 1pt)
    """
    // println(s"preview main $content")
    Typst
      .previewSvg(content, packageSources)
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
  val gridSettingsVar = Var(GridSettings())
  val assetLibraryStateVar = Var(AssetLibraryState())
  val assetLibraryStateSignal = assetLibraryStateVar.signal

  private def loadedAssetPackages: List[LoadedAssetPackage] =
    assetLibraryStateVar.now().packages

  private def allAssetResources: List[AssetResource] =
    BuiltinAssets.resources ++ loadedAssetPackages.flatMap(_.resources)

  def loadAssetIndex(url: String): Unit = {
    val trimmedUrl = url.trim
    if trimmedUrl.isEmpty then
      assetLibraryStateVar.update(
        _.copy(sourceUrl = "", loading = false, error = Some("Asset index URL is empty")),
      )
      return

    assetLibraryStateVar.update(
      _.copy(sourceUrl = trimmedUrl, loading = true, error = None),
    )
    AssetLibraryLoader
      .loadIndex(trimmedUrl)
      .`then`((packages: List[LoadedAssetPackage]) => {
        assetLibraryStateVar.set(
          AssetLibraryState(
            sourceUrl = trimmedUrl,
            loading = false,
            packages = packages,
            error = None,
          ),
        )
        updatePreview()
      })
      .`catch`((error: Any) => {
        assetLibraryStateVar.update(
          _.copy(loading = false, error = Some(errorMessage(error))),
        )
      })
  }

  def insertResource(resourceId: String, name: String): Unit = {
    val resource = allAssetResources.find(_.id == resourceId) match {
      case Some(resource) => resource
      case None =>
        assetLibraryStateVar.update(
          _.copy(error = Some(s"Unknown asset resource: $resourceId")),
        )
        return
    }
    val nodeName =
      if name.trim.nonEmpty then name.trim
      else s"node-${math.random().toString.replace("0.", "").take(6)}"
    val args = resource.args
      .map(arg => s"${arg.name}: ${arg.defaultValue}")
      .mkString(", ")
    val item =
      s"""make-ins("${escapeStr(nodeName)}", (0, 0), "${escapeStr(resource.id)}", ($args))"""
    programContentVar.update(content => appendMakeIns(content, item))
  }

  def updateProgram() = {
    var content = instances
      .map { ins =>
        val args = ins.extraArgs.iterator.flatten.map(_.repr).mkString(", ")
        s"""make-ins("${ins.name}", (${ins.pos.get._1}, ${ins.pos.get._2}), "${ins.ty}", (${args}))"""
      }
      .mkString("#{", "\n  ", "}")

    programContentVar.update(_ => content)
  }

  private def appendMakeIns(content: String, item: String): String = {
    val trimmed = content.trim
    if trimmed.startsWith("#{") && trimmed.endsWith("}") then
      val body = trimmed.drop(2).dropRight(1).trim
      if body.isEmpty then s"#{\n  $item\n}"
      else s"#{\n  $body\n  $item\n}"
    else s"#{\n  $item\n}"
  }

  private def errorMessage(error: Any): String =
    Option(error).fold("Unknown asset library error")(_.toString)

  def onPreviewMounted(panel: dom.Element): Unit = {
    panel.addEventListener(
      "mousedown",
      e => this.startDrag(panel, e.asInstanceOf[dom.MouseEvent]),
    );
    panel.addEventListener(
      "mousemove",
      e => this.drag(panel, e.asInstanceOf[dom.MouseEvent]),
    );

    panel.addEventListener(
      "mouseup",
      e => this.endDrag(panel, e.asInstanceOf[dom.MouseEvent]),
    );
    panel.addEventListener(
      "mouseleave",
      e => this.endDrag(panel, e.asInstanceOf[dom.MouseEvent]),
    );
    // panel.addEventListener(
    //   "contextmenu",
    //   e => this.doToggleContextMenu(panel, e),
    // );
  }

  def syncGridMetrics(panel: dom.Element): Unit = {
    val settings = gridSettingsVar.now()
    val fallback = {
      val size = Grid.clean(settings.size)
      (0d, 0d, size, size)
    }
    val shapeMetrics = shapeGridMetrics(panel)
    val rootMetrics = svgRootGridMetrics(panel)

    val metrics = shapeMetrics.orElse(rootMetrics).getOrElse(fallback)
    val originX = metrics._1
    val originY = metrics._2
    val sizeX = metrics._3
    val sizeY = metrics._4

    val style = panel.asInstanceOf[js.Dynamic].style
    style.setProperty("--vistyp-grid-size-x", s"${Grid.clean(sizeX)}px")
    style.setProperty("--vistyp-grid-size-y", s"${Grid.clean(sizeY)}px")
    style.setProperty("--vistyp-grid-origin-x", s"${Grid.clean(originX)}px")
    style.setProperty("--vistyp-grid-origin-y", s"${Grid.clean(originY)}px")
  }

  private def shapeGridMetrics(
      panel: dom.Element,
  ): Option[(Double, Double, Double, Double)] = {
    val instanceById = instances
      .flatMap(ins => ins.initPos.orElse(ins.pos).map(pos => ins.name -> (ins, pos)))
      .toMap
    val elements = panel.querySelectorAll(".typst-cetz-elem")
    val panelRect = panel.getBoundingClientRect()
    val panelDynamic = panel.asInstanceOf[js.Dynamic]
    val scrollLeft = panelDynamic.scrollLeft.asInstanceOf[Double]
    val scrollTop = panelDynamic.scrollTop.asInstanceOf[Double]
    val settings = gridSettingsVar.now()

    val rootMetrics = svgRootGridMetrics(panel)
    val unitWidth = rootMetrics.map(_._3 / settings.size).getOrElse(1d)
    val unitHeight = rootMetrics.map(_._4 / settings.size).getOrElse(1d)
    if (unitWidth <= 0 || unitHeight <= 0) {
      return None
    }

    def findMetrics(
        preferZeroOffset: Boolean,
    ): Option[(Double, Double, Double, Double)] = {
      var idx = 0
      while (idx < elements.length) {
        val element = elements(idx).asInstanceOf[dom.Element]
        val elementId = getTypstElementId(element)
        instanceById.get(elementId) match {
          case Some((ins, (posX, posY))) =>
            val offset = snapReferenceOffset(ins)
            val zeroOffset = offset._1 == 0 && offset._2 == 0
            if (!preferZeroOffset || zeroOffset) {
              elementAnchorPoint(element, ins).foreach {
                case (anchorX, anchorY) =>
                  val originX =
                    anchorX - panelRect.left + scrollLeft - posX * unitWidth
                  val originY =
                    anchorY - panelRect.top + scrollTop + posY * unitHeight
                  val gridSizeX = settings.size * unitWidth
                  val gridSizeY = settings.size * unitHeight
                  return Some((originX, originY, gridSizeX, gridSizeY))
              }
            }
          case None =>
        }

        idx += 1
      }

      None
    }

    findMetrics(preferZeroOffset = true).orElse(findMetrics(preferZeroOffset = false))
  }

  private def elementAnchorPoint(
      element: dom.Element,
      ins: Instance,
  ): Option[(Double, Double)] = {
    val shape = element.querySelector(".typst-shape").asInstanceOf[dom.Element | Null]
    if (shape == null) {
      return None
    }

    val rect = shape.getBoundingClientRect()
    if (rect.width <= 0 || rect.height <= 0) {
      return None
    }

    ins.ty match {
      case "x-circle" =>
        Some(((rect.left + rect.right) / 2) -> ((rect.top + rect.bottom) / 2))
      case _ =>
        Some(rect.left -> rect.bottom)
    }
  }

  private def svgRootGridMetrics(
      panel: dom.Element,
  ): Option[(Double, Double, Double, Double)] = {
    val svg = panel.querySelector("svg").asInstanceOf[dom.SVGElement | Null]
    if (svg == null) {
      return None
    }

    val ctmRaw = svg.asInstanceOf[js.Dynamic].getScreenCTM()
    if (ctmRaw == null || js.isUndefined(ctmRaw)) {
      return None
    }

    val ctm = ctmRaw.asInstanceOf[dom.SVGMatrix]
    val panelRect = panel.getBoundingClientRect()
    val panelDynamic = panel.asInstanceOf[js.Dynamic]
    val scrollLeft = panelDynamic.scrollLeft.asInstanceOf[Double]
    val scrollTop = panelDynamic.scrollTop.asInstanceOf[Double]
    val settings = gridSettingsVar.now()
    Some(
      (
        ctm.e - panelRect.left + scrollLeft,
        ctm.f - panelRect.top + scrollTop,
        settings.size * math.abs(ctm.a),
        settings.size * math.abs(ctm.d),
      ),
    )
  }

  var selectedElem: Option[dom.Element] = None
  var offset: (Double, Double) = (0, 0)
  var svgElem: Option[dom.SVGElement] = None
  var transform = Option.empty[dom.SVGTransform]
  var dragStartPos: Option[(Double, Double)] = None
  def startDrag(panel: dom.Element, evt: dom.MouseEvent): Unit = {

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
    evt.preventDefault()
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
    dragStartPos =
      instances.find(_.name == typstId).flatMap(_.pos).orElse(Some(0d -> 0d))
    instances = instances.map { ins =>
      if (ins.name == typstId) {
        ins.copy(initPos = dragStartPos)
      } else {
        ins
      }
    }
  }

  def drag(panel: dom.Element, evt: dom.MouseEvent): Unit = {
    val selectedElement = selectedElem match {
      case Some(selectedElem) => selectedElem
      case None               => return
    }

    evt.preventDefault()
    val coord = getMousePosition(evt)
    val rawX = coord._1 - offset._1
    val rawY = coord._2 - offset._2
    val typstId = getTypstElementId(selectedElement)
    val (x, y) = snappedDragOffset(rawX, rawY, evt.altKey, typstId)
    transform.get.setTranslate(x, y)
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

  def endDrag(panel: dom.Element, evt: dom.MouseEvent): Unit = {
    val selectedElement = selectedElem match {
      case Some(selectedElem) => selectedElem
      case None               => return
    }
    selectedElem = None
    dragStartPos = None

    val typstId = getTypstElementId(selectedElement)
    println(s"find typstId $typstId in $instances")
    val ins = instances.find(_.name == typstId).get
    if (ins.pos.isDefined) {
      val initPos = ins.initPos.getOrElse(0d -> 0d)
      val x = Grid.clean(initPos._1 + ins.pos.get._1)
      val y = Grid.clean(initPos._2 - ins.pos.get._2)
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

  private def snappedDragOffset(
      rawX: Double,
      rawY: Double,
      bypassSnap: Boolean,
      typstId: String,
  ): (Double, Double) = {
    val origin = dragStartPos.getOrElse(0d -> 0d)
    val settings = gridSettingsVar.now()
    if (!Grid.active(settings) || bypassSnap) {
      Grid.clean(rawX) -> Grid.clean(rawY)
    } else {
      val snappedPosition = Grid.snapPointWithOffset(
        (origin._1 + rawX) -> (origin._2 - rawY),
        snapReferenceOffset(typstId),
        settings,
      )
      Grid.clean(snappedPosition._1 - origin._1) ->
        Grid.clean(origin._2 - snappedPosition._2)
    }
  }

  private def snapReferenceOffset(typstId: String): (Double, Double) =
    instances
      .find(_.name == typstId)
      .map(snapReferenceOffset)
      .getOrElse(0d -> 0d)

  private def snapReferenceOffset(ins: Instance): (Double, Double) =
    ins.ty match {
      case "x-circle" =>
        val radius = numberArg(ins, "rad").getOrElse(200d)
        (-radius) -> (-radius)
      case _ => 0d -> 0d
    }

  private def numberArg(ins: Instance, key: String): Option[Double] =
    ins.extraArgs.toList.flatten.collectFirst {
      case KeyedArg(Ident(argName), value) if argName == key => getNumber(value)
    }

  def getTypstElementId(target: dom.Element): String = {
    target.getAttribute("data-element-id").replace("cetz-app-", "")
  }

  /** A Fetch Action from DOM
    */
  def getMousePosition(evt: dom.MouseEvent) = {
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
