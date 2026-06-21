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
  private var previewResizeObserver: Option[dom.ResizeObserver] = None
  private val previewZoomLevels =
    Vector(0.1d, 0.25d, 0.5d, 0.75d, 1d, 1.25d, 1.5d, 2d, 3d, 4d)
  private var previewZoomIndex: Int = previewZoomLevels.indexOf(1d)

  private def previewZoom: Double = previewZoomLevels(previewZoomIndex)

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

    val (packageImports, packageAliases) = genPackageImports()
    val packageSources = loadedAssetPackages.flatMap(_.files).toMap

    val content = s"""
    #import "@preview/cetz:0.5.2"
    // #place(dx: 0pt, dy: 0pt, circle())
    // #place(dx: 50pt, dy: 50pt, square())
    // #place(dx: 200pt, dy: 0pt, circle())
    // #place(dx: 50pt, dy: 50pt, square())
    
    #set page(width: auto, height: auto, margin: 12pt)
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

  private def resourceById(resourceId: String): Option[AssetResource] =
    allAssetResources.find(_.id == resourceId)

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
    val resource = resourceById(resourceId) match {
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
    val content = appendMakeIns(programContentVar.now(), item)
    programContentVar.set(content)
    updateDiagram(content)
  }

  def elementIdAt(target: dom.EventTarget): Option[String] =
    try
      findTaggedTypstElement(target.asInstanceOf[dom.Element]).map(getTypstElementId)
    catch case _: Throwable => None

  def deleteElement(elementId: String): Unit = {
    val nextInstances = instances.filterNot(_.name == elementId)
    if nextInstances.length == instances.length then return

    selectedElem = None
    dragStartPos = None
    transform = None
    instances = nextInstances
    updateProgram()
  }

  def elementProperties(elementId: String): Option[ElementPropertyState] =
    instances.find(_.name == elementId).map { ins =>
      val resource = resourceById(ins.ty)
      val resourceArgs = resource.map(_.args).getOrElse(Nil)
      val resourceArgNames = resourceArgs.map(_.name).toSet
      val extraArgs = ins.extraArgs.getOrElse(Nil)
      val keyedArgs = extraArgs.collect {
        case KeyedArg(Ident(name), value) => name -> value
      }.toMap
      val fields = resourceArgs.map { arg =>
        propertyField(arg, keyedArgs.get(arg.name))
      }
      val rawExtraArgs = extraArgs
        .filterNot(arg => keyedArgName(arg).exists(resourceArgNames.contains))
        .map(_.repr)
        .mkString(", ")
      val pos = ins.pos.getOrElse(0d -> 0d)

      ElementPropertyState(
        elementId = ins.name,
        resourceId = ins.ty,
        resourceLabel = resource.map(resourceDisplayLabel).getOrElse(ins.ty),
        sourceCode = elementSource(ins),
        positionX = Grid.clean(pos._1).toString,
        positionY = Grid.clean(pos._2).toString,
        fields = fields,
        extraArgs = rawExtraArgs,
        error = None,
      )
    }

  def saveElementProperties(
      state: ElementPropertyState,
  ): Either[String, ElementPropertyState] = {
    val position = parsePosition(state)
    val knownArgs = parsePropertyFields(state.fields)
    val extraArgs = parseExtraArgs(state.extraArgs)

    (position, knownArgs, extraArgs) match
      case (Left(error), _, _) => Left(error)
      case (_, Left(error), _) => Left(error)
      case (_, _, Left(error)) => Left(error)
      case (Right(pos), Right(known), Right(extra)) =>
        instances.find(_.name == state.elementId) match
          case None => Left("Element no longer exists")
          case Some(_) =>
            val args = known ++ extra
            duplicatePropertyName(args) match
              case Some(name) => Left(s"Duplicate property: $name")
              case None =>
                instances = instances.map { ins =>
                  if ins.name == state.elementId then
                    ins.copy(pos = Some(pos), extraArgs = Some(args), initPos = None)
                  else ins
                }
                updateProgram()
                elementProperties(state.elementId).toRight("Element no longer exists")
  }

  def saveElementSource(
      elementId: String,
      sourceCode: String,
  ): Either[String, ElementPropertyState] =
    parseElementSource(sourceCode).flatMap { nextInstance =>
      if !instances.exists(_.name == elementId) then Left("Element no longer exists")
      else if nextInstance.name != elementId && instances.exists(_.name == nextInstance.name)
      then Left(s"Duplicate element name: ${nextInstance.name}")
      else
        instances = instances.map { ins =>
          if ins.name == elementId then nextInstance else ins
        }
        updateProgram()
        elementProperties(nextInstance.name).toRight("Element no longer exists")
    }

  def updateProgram() = {
    val body = instances.map { ins =>
      instanceSource(ins)
    }
    val content =
      if body.isEmpty then "#{\n}"
      else body.mkString("#{", "\n  ", "}")

    programContentVar.set(content)
    updatePreview()
  }

  private def resourceDisplayLabel(resource: AssetResource): String =
    s"${resource.functionName} · ${resource.packageLabel}"

  private def instanceSource(ins: Instance): String =
    val pos = ins.pos.getOrElse(0d -> 0d)
    val args = ins.extraArgs.iterator.flatten.map(_.repr).mkString(", ")
    s"""make-ins("${escapeStr(ins.name)}", (${pos._1}, ${pos._2}), "${escapeStr(ins.ty)}", ($args))"""

  private def elementSource(ins: Instance): String =
    s"#${instanceSource(ins)}"

  private def parseElementSource(sourceCode: String): Either[String, Instance] =
    try
      val expression = sourceCode.trim.stripPrefix("#").trim
      constructInstance(parseCodeExpression(expression)) match
        case Some(instance) => Right(instance)
        case None          => Left("Element source must be a make-ins(...) call")
    catch case error: Throwable => Left(s"Invalid element source: ${error.getMessage}")

  private def propertyField(
      arg: AssetArg,
      value: Option[syntax.Node],
  ): ElementPropertyField =
    val (displayValue, textLiteral) = propertyDisplayValue(arg, value)
    ElementPropertyField(
      name = arg.name,
      value = displayValue,
      kind = arg.kind,
      textLiteral = textLiteral,
    )

  private def propertyDisplayValue(
      arg: AssetArg,
      value: Option[syntax.Node],
  ): (String, Boolean) =
    value match
      case Some(StrLit(value)) if arg.kind == "text" => value -> true
      case Some(node)                                => node.repr -> false
      case None =>
        try
          parseCodeExpression(arg.defaultValue) match
            case StrLit(value) if arg.kind == "text" => value -> true
            case node                                => node.repr -> false
        catch case _: Throwable => arg.defaultValue -> false

  private def parsePosition(
      state: ElementPropertyState,
  ): Either[String, (Double, Double)] =
    (state.positionX.trim.toDoubleOption, state.positionY.trim.toDoubleOption) match
      case (Some(x), Some(y)) => Right(Grid.clean(x) -> Grid.clean(y))
      case (None, _)         => Left("Invalid X position")
      case (_, None)         => Left("Invalid Y position")

  private def parsePropertyFields(
      fields: List[ElementPropertyField],
  ): Either[String, List[syntax.Node]] =
    fields.foldLeft[Either[String, List[syntax.Node]]](Right(Nil)) {
      case (Left(error), _) => Left(error)
      case (Right(args), field) =>
        parsePropertyField(field).map(arg => args :+ arg)
    }

  private def parsePropertyField(
      field: ElementPropertyField,
  ): Either[String, syntax.Node] =
    val value = field.value.trim
    if field.kind == "text" && field.textLiteral then
      Right(KeyedArg(Ident(field.name), StrLit(field.value)))
    else if value.isEmpty then Left(s"${field.name} is empty")
    else
      parsePropertyValue(field.name, value).map { node =>
        KeyedArg(Ident(field.name), node)
      }

  private def parseExtraArgs(raw: String): Either[String, List[syntax.Node]] =
    val trimmed = raw.trim
    if trimmed.isEmpty then Right(Nil)
    else
      val source =
        if trimmed.startsWith("(") && trimmed.endsWith(")") then trimmed
        else s"($trimmed)"
      parsePropertyValue("extra arguments", source).flatMap {
        case ArgsLit(values) => Right(values)
        case _ => Left("Extra arguments must be a comma-separated argument list")
      }

  private def parsePropertyValue(
      label: String,
      source: String,
  ): Either[String, syntax.Node] =
    try Right(parseCodeExpression(source))
    catch case error: Throwable => Left(s"Invalid $label: ${error.getMessage}")

  private def duplicatePropertyName(args: List[syntax.Node]): Option[String] =
    args
      .flatMap(keyedArgName)
      .groupBy(identity)
      .collectFirst { case (name, values) if values.length > 1 => name }

  private def keyedArgName(arg: syntax.Node): Option[String] =
    arg match
      case KeyedArg(Ident(name), _) => Some(name)
      case _                        => None

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
    previewResizeObserver.foreach(_.disconnect())
    val observer = new dom.ResizeObserver((_, _) => schedulePreviewLayoutSync(panel))
    observer.observe(panel)
    previewResizeObserver = Some(observer)
    schedulePreviewLayoutSync(panel)

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
    panel.addEventListener(
      "wheel",
      e => this.zoomPreview(panel, e.asInstanceOf[dom.WheelEvent]),
    );
    // panel.addEventListener(
    //   "contextmenu",
    //   e => this.doToggleContextMenu(panel, e),
    // );
  }

  private def schedulePreviewLayoutSync(panel: dom.Element): Unit = {
    dom.window.requestAnimationFrame { _ =>
      applyPreviewZoom(panel)
      syncGridMetrics(panel)
    }
  }

  def applyPreviewZoom(panel: dom.Element): Unit = {
    val svg = panel.querySelector("svg").asInstanceOf[dom.SVGSVGElement | Null]
    if (svg == null) {
      return
    }

    svgNaturalSize(svg) match {
      case Some((naturalWidth, naturalHeight)) =>
        val scale = previewZoom

        if (scale <= 0 || scale.isNaN || scale.isInfinity) {
          return
        }

        val svgStyle = svg.asInstanceOf[js.Dynamic].style
        svgStyle.setProperty("width", s"${Grid.clean(naturalWidth * scale)}px")
        svgStyle.setProperty("height", s"${Grid.clean(naturalHeight * scale)}px")
        svg.setAttribute("preserveAspectRatio", "xMidYMid meet")

      case None =>
    }
  }

  private def zoomPreview(panel: dom.Element, evt: dom.WheelEvent): Unit = {
    if (!evt.ctrlKey) {
      return
    }

    evt.preventDefault()
    if (evt.deltaY == 0) {
      return
    }

    val nextIndex =
      if evt.deltaY < 0 then previewZoomIndex + 1 else previewZoomIndex - 1
    val clampedIndex = nextIndex.max(0).min(previewZoomLevels.length - 1)
    if (clampedIndex == previewZoomIndex) {
      return
    }

    val oldZoom = previewZoom
    val panelRect = panel.getBoundingClientRect()
    val panelDynamic = panel.asInstanceOf[js.Dynamic]
    val pointerX = evt.clientX - panelRect.left
    val pointerY = evt.clientY - panelRect.top
    val scrollLeft = panelDynamic.scrollLeft.asInstanceOf[Double]
    val scrollTop = panelDynamic.scrollTop.asInstanceOf[Double]
    val contentX = scrollLeft + pointerX
    val contentY = scrollTop + pointerY

    previewZoomIndex = clampedIndex
    applyPreviewZoom(panel)

    val ratio = previewZoom / oldZoom
    panelDynamic.updateDynamic("scrollLeft")(
      math.max(0d, contentX * ratio - pointerX),
    )
    panelDynamic.updateDynamic("scrollTop")(
      math.max(0d, contentY * ratio - pointerY),
    )
    syncGridMetrics(panel)
  }

  private def svgNaturalSize(svg: dom.SVGSVGElement): Option[(Double, Double)] = {
    val viewBox = svg.viewBox.baseVal
    if (viewBox != null && viewBox.width > 0 && viewBox.height > 0) {
      return Some(viewBox.width -> viewBox.height)
    }

    val width = svg.width.baseVal.value
    val height = svg.height.baseVal.value
    if (width > 0 && height > 0) Some(width -> height)
    else None
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
  var dragMoved: Boolean = false
  def startDrag(panel: dom.Element, evt: dom.MouseEvent): Unit = {
    if evt.button != 0 then return

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
    dragMoved = false
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
    if !dragMoved && math.abs(rawX) < 0.5 && math.abs(rawY) < 0.5 then return

    dragMoved = true
    val typstId = getTypstElementId(selectedElement)
    val (x, y) = snappedDragOffset(rawX, rawY, evt.altKey, typstId)
    transform.get.setTranslate(x, y)
    println(s"find typstId $typstId in $instances")
    instances = instances.map { ins =>
      if (ins.name == typstId) {
        ins.copy(pos = Some(x -> y), initPos = dragStartPos)
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
    val shouldCommitDrag = dragMoved
    dragMoved = false
    dragStartPos = None
    if !shouldCommitDrag then return

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

case class ElementPropertyField(
    name: String,
    value: String,
    kind: String,
    textLiteral: Boolean = false,
)

case class ElementPropertyState(
    elementId: String,
    resourceId: String,
    resourceLabel: String,
    sourceCode: String,
    positionX: String,
    positionY: String,
    fields: List[ElementPropertyField],
    extraArgs: String,
    error: Option[String] = None,
)
