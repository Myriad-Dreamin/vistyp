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
  private var previewPanelElement: Option[dom.Element] = None
  val connectionToolStateVar = Var(ConnectionToolState())
  val connectionToolStateSignal = connectionToolStateVar.signal
  private var selectedInstanceId: Option[String] = None
  val selectedInstanceVar = Var(Option.empty[SelectedInstanceDetails])
  val selectedInstanceSignal = selectedInstanceVar.signal
  private var contextMenuElement: Option[dom.Element] = None
  private var contextMenuDismissInstalled = false
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

    val startStillExists =
      connectionToolStateVar.now().startElementId.forall(start =>
        newInstances.exists(_.name == start),
      )
    if !startStillExists then
      connectionToolStateVar.update(_.copy(startElementId = None))

    instances = newInstances
    refreshSelectedInstance()
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
  let edge-name = if node-label == "" {
    "t"
  } else {
    node-label
  }
  line(start, end, name: edge-name, mark: mark)
  content(edge-name + ".centroid", inner-text)
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

    val packageSources = loadedAssetPackages.flatMap(_.files).toMap

    val content = generatedTypstSource
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

  def previewSvgNow: String = previewContentVar.now()._1

  def generatedTypstSource: String = {
    val (packageImports, packageAliases) = genPackageImports()

    s"""
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
  }

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
    val content = appendMakeIns(programContentVar.now(), item)
    programContentVar.set(content)
    updateDiagram(content)
  }

  def clearInstanceSelection(): Unit = {
    selectedInstanceId = None
    selectedInstanceVar.set(None)
    previewPanelElement.foreach(syncInstanceSelection)
    hideContextMenu()
  }

  def updateInstanceName(instanceId: String, newName: String): Unit = {
    val trimmed = newName.trim
    if trimmed.isEmpty then
      setPropertyError("Instance name cannot be empty")
      return
    if trimmed != instanceId && instances.exists(_.name == trimmed) then
      setPropertyError(s"Instance name already exists: $trimmed")
      return

    instances = instances.map { ins =>
      val renamed =
        if ins.name == instanceId then ins.copy(name = trimmed)
        else ins
      if trimmed == instanceId then renamed
      else renamed.copy(extraArgs = updateReferenceArgs(renamed.extraArgs, instanceId, trimmed))
    }
    selectedInstanceId = Some(trimmed)
    commitInstances(trimmed)
  }

  def updateInstancePosition(
      instanceId: String,
      rawX: String,
      rawY: String,
  ): Unit = {
    val nextPos = for {
      x <- rawX.trim.toDoubleOption
      y <- rawY.trim.toDoubleOption
    } yield Grid.clean(x) -> Grid.clean(y)

    nextPos match {
      case Some(pos) =>
        instances = instances.map { ins =>
          if ins.name == instanceId then ins.copy(pos = Some(pos), initPos = None)
          else ins
        }
        commitInstances(instanceId)
      case None =>
        setPropertyError(s"Invalid position: ($rawX, $rawY)")
    }
  }

  def updateInstanceArg(
      instanceId: String,
      argName: String,
      rawValue: String,
  ): Unit = {
    val instance = instances.find(_.name == instanceId) match {
      case Some(instance) => instance
      case None =>
        clearInstanceSelection()
        return
    }
    val kind = resourceArg(instance, argName).map(_.kind).getOrElse("code")
    val parsedValue = parsePropertyValue(rawValue, kind)

    parsedValue match {
      case Right(value) =>
        instances = instances.map { ins =>
          if ins.name == instanceId then
            ins.copy(extraArgs = Some(upsertArg(ins.extraArgs.toList.flatten, argName, value)))
          else ins
        }
        commitInstances(instanceId)
      case Left(error) =>
        setPropertyError(s"Invalid $argName: $error")
    }
  }

  def deleteInstance(instanceId: String): Unit = {
    instances = instances.filterNot(_.name == instanceId)
    if selectedInstanceId.contains(instanceId) then selectedInstanceId = None
    selectedInstanceVar.set(None)
    hideContextMenu()
    updateProgram()
    updatePreview()
    previewPanelElement.foreach(syncInstanceSelection)
  }

  def toggleConnectionTool(): Unit =
    setConnectionToolEnabled(!connectionToolStateVar.now().enabled)

  private def setConnectionToolEnabled(enabled: Boolean): Unit = {
    connectionToolStateVar.set(ConnectionToolState(enabled = enabled))
    previewPanelElement.foreach(syncConnectionTool)
  }

  private def insertConnection(startId: String, endId: String): Unit = {
    val start = instances.find(_.name == startId)
    val end = instances.find(_.name == endId)
    (start, end) match {
      case (Some(startIns), Some(endIns)) =>
        val resource = chooseConnectionResource(startIns, endIns)
        val (startAnchor, endAnchor) = connectionAnchors(startIns, endIns)
        val name = uniqueConnectionName(startId, endId)
        val item =
          s"""make-ins("${escapeStr(name)}", (0, 0), "${escapeStr(resource.id)}", (start: "${escapeStr(startAnchor)}", end: "${escapeStr(endAnchor)}"))"""
        val content = appendMakeIns(programContentVar.now(), item)
        programContentVar.set(content)
        updateDiagram(content)

      case _ =>
        connectionToolStateVar.update(_.copy(startElementId = None))
    }
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

  private def commitInstances(selectionId: String): Unit = {
    selectedInstanceId = Some(selectionId)
    updateProgram()
    refreshSelectedInstance()
    updatePreview()
    previewPanelElement.foreach(syncInstanceSelection)
  }

  private def refreshSelectedInstance(): Unit =
    selectedInstanceId match {
      case Some(instanceId) =>
        instances.find(_.name == instanceId) match {
          case Some(instance) =>
            selectedInstanceVar.set(Some(instanceDetails(instance)))
          case None =>
            selectedInstanceId = None
            selectedInstanceVar.set(None)
        }
      case None =>
        selectedInstanceVar.set(None)
    }

  private def instanceDetails(instance: Instance): SelectedInstanceDetails = {
    val pos = instance.pos.getOrElse(0d -> 0d)
    SelectedInstanceDetails(
      id = instance.name,
      name = instance.name,
      ty = instance.ty,
      x = Grid.clean(pos._1).toString,
      y = Grid.clean(pos._2).toString,
      args = instanceArgDetails(instance),
      isConnection = isConnectionInstance(instance),
    )
  }

  private def instanceArgDetails(instance: Instance): List[SelectedInstanceArg] = {
    val keyedArgs = instance.extraArgs.toList.flatten.collect {
      case arg @ KeyedArg(Ident(argName), value) => argName -> (arg, value)
    }
    val keyedByName = keyedArgs.toMap
    val resourceArgs = resourceForInstance(instance).map(_.args).getOrElse(Nil)
    val orderedNames =
      (resourceArgs.map(_.name) ++ keyedArgs.map(_._1)).distinct

    orderedNames.map { argName =>
      val resourceArgOpt = resourceArgs.find(_.name == argName)
      val valueOpt = keyedByName.get(argName).map(_._2)
      val fallbackValue =
        resourceArgOpt
          .flatMap(arg => parseCodeExpression(arg.defaultValue).toOption)
      val kind = resourceArgOpt
        .map(_.kind)
        .orElse(valueOpt.map(valueKind))
        .getOrElse("code")
      val value = valueOpt
        .orElse(fallbackValue)
        .map(displayPropertyValue(_, kind))
        .getOrElse("")

      SelectedInstanceArg(
        name = argName,
        kind = kind,
        value = value,
        present = valueOpt.isDefined,
      )
    }
  }

  private def resourceForInstance(instance: Instance): Option[AssetResource] =
    allAssetResources.find(_.id == instance.ty)

  private def resourceArg(instance: Instance, argName: String): Option[AssetArg] =
    resourceForInstance(instance).flatMap(_.args.find(_.name == argName))

  private def displayPropertyValue(value: syntax.Node, kind: String): String =
    value match {
      case StrLit(text) if kind == "text" => text
      case _                              => value.repr
    }

  private def valueKind(value: syntax.Node): String =
    value match {
      case StrLit(_)              => "text"
      case BoolLit(_)             => "boolean"
      case IntLit(_) | FloatLit(_) => "number"
      case _                      => "code"
    }

  private def parsePropertyValue(
      rawValue: String,
      kind: String,
  ): Either[String, syntax.Node] =
    if kind == "text" then Right(StrLit(rawValue))
    else
      val trimmed = rawValue.trim
      if trimmed.isEmpty then Left("value is empty")
      else parseCodeExpression(trimmed)

  private def upsertArg(
      args: List[syntax.Node],
      argName: String,
      value: syntax.Node,
  ): List[syntax.Node] = {
    val next = KeyedArg(Ident(argName), value)
    var replaced = false
    val updated = args.map {
      case KeyedArg(Ident(name), _) if name == argName =>
        replaced = true
        next
      case other => other
    }
    if replaced then updated else updated :+ next
  }

  private def updateReferenceArgs(
      extraArgs: Option[List[syntax.Node]],
      oldName: String,
      newName: String,
  ): Option[List[syntax.Node]] =
    extraArgs.map(_.map {
      case KeyedArg(Ident(argName), StrLit(value))
          if argName == "start" || argName == "end" =>
        KeyedArg(Ident(argName), StrLit(rewriteAnchorReference(value, oldName, newName)))
      case other => other
    })

  private def rewriteAnchorReference(
      value: String,
      oldName: String,
      newName: String,
  ): String =
    if value == oldName then newName
    else if value.startsWith(oldName + ".") then newName + value.drop(oldName.length)
    else value

  private def setPropertyError(message: String): Unit =
    assetLibraryStateVar.update(_.copy(error = Some(message)))

  private def chooseConnectionResource(
      start: Instance,
      end: Instance,
  ): AssetResource = {
    val candidates = allAssetResources.filter(isConnectionResource)
    val endpointKeys = List(start.ty, end.ty)
      .flatMap(ResourceRef.parse)
      .map(_.packageKey)
      .distinct
    val diagramKeys = instances
      .flatMap(ins => ResourceRef.parse(ins.ty).map(_.packageKey))
      .distinct

    bestConnectionResource(
      candidates.filter(resource =>
        resource.packageKey.exists(endpointKeys.contains),
      ),
      start,
      end,
    ).orElse(
      bestConnectionResource(
        candidates.filter(resource =>
          resource.packageKey.exists(diagramKeys.contains),
        ),
        start,
        end,
      ),
    ).orElse(candidates.find(_.id == "x-arrow"))
      .orElse(BuiltinAssets.resources.find(_.id == "x-arrow"))
      .get
  }

  private def bestConnectionResource(
      candidates: List[AssetResource],
      start: Instance,
      end: Instance,
  ): Option[AssetResource] = {
    val bothConnectionLike = isConnectionInstance(start) && isConnectionInstance(end)
    val byName = candidates.sortBy(resource =>
      connectionResourceRank(resource, bothConnectionLike),
    )
    byName.headOption
  }

  private def connectionResourceRank(
      resource: AssetResource,
      bothConnectionLike: Boolean,
  ): Int = {
    val name = resource.functionName.toLowerCase
    if bothConnectionLike && name.contains("2cell") then 0
    else if bothConnectionLike && name.contains("cell") then 1
    else if name.contains("arrow") then 2
    else if name.contains("edge") then 3
    else 4
  }

  private def isConnectionResource(resource: AssetResource): Boolean = {
    val argNames = resource.args.map(_.name).toSet
    argNames.contains("start") && argNames.contains("end")
  }

  private def connectionAnchors(
      start: Instance,
      end: Instance,
  ): (String, String) = {
    if isConnectionInstance(start) && isConnectionInstance(end) then
      return s"${start.name}.centroid" -> s"${end.name}.centroid"

    val (startSide, endSide) = connectionSides(start, end)
    connectionAnchor(start, startSide) -> connectionAnchor(end, endSide)
  }

  private def connectionAnchor(ins: Instance, side: String): String =
    if isConnectionInstance(ins) then s"${ins.name}.centroid"
    else s"${ins.name}.$side"

  private def connectionSides(start: Instance, end: Instance): (String, String) = {
    val startPos = start.pos.getOrElse(0d -> 0d)
    val endPos = end.pos.getOrElse(0d -> 0d)
    val dx = endPos._1 - startPos._1
    val dy = endPos._2 - startPos._2
    val absX = math.abs(dx)
    val absY = math.abs(dy)

    if absX == 0 && absY == 0 then "east" -> "west"
    else if absX > absY * 1.25 then
      if dx >= 0 then "east" -> "west" else "west" -> "east"
    else if absY > absX * 1.25 then
      if dy >= 0 then "north" -> "south" else "south" -> "north"
    else if dx >= 0 && dy >= 0 then "north-east" -> "south-west"
    else if dx >= 0 then "south-east" -> "north-west"
    else if dy >= 0 then "north-west" -> "south-east"
    else "south-west" -> "north-east"
  }

  private def isConnectionInstance(ins: Instance): Boolean = {
    val argNames = keyedArgNames(ins)
    val functionName = ResourceRef.parse(ins.ty).map(_.functionName).getOrElse(ins.ty)
    (argNames.contains("start") && argNames.contains("end")) ||
    functionName.toLowerCase.contains("arrow")
  }

  private def keyedArgNames(ins: Instance): Set[String] =
    ins.extraArgs.toList.flatten.collect {
      case KeyedArg(Ident(argName), _) => argName
    }.toSet

  private def uniqueConnectionName(startId: String, endId: String): String = {
    val base = sanitizeConnectionName(s"a-$startId-$endId")
    val used = instances.map(_.name).toSet
    if !used.contains(base) then base
    else
      LazyList
        .from(2)
        .map(idx => s"$base-$idx")
        .find(name => !used.contains(name))
        .get
  }

  private def sanitizeConnectionName(raw: String): String = {
    val normalized = raw
      .map(ch =>
        if ch.isLetterOrDigit || ch == '-' || ch == '_' then ch
        else '-',
      )
      .mkString
      .replaceAll("-+", "-")
      .stripPrefix("-")
      .stripSuffix("-")

    if normalized.nonEmpty then normalized else "edge"
  }

  private def errorMessage(error: Any): String =
    Option(error).fold("Unknown asset library error")(_.toString)

  def onPreviewMounted(panel: dom.Element): Unit = {
    previewPanelElement = Some(panel)
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
    panel.addEventListener(
      "contextmenu",
      e => this.showInstanceContextMenu(panel, e.asInstanceOf[dom.MouseEvent]),
    );
    if !contextMenuDismissInstalled then
      contextMenuDismissInstalled = true
      dom.document.addEventListener(
        "mousedown",
        e => {
          val target = e.target.asInstanceOf[dom.Element | Null]
          val insideMenu = contextMenuElement.exists(menu =>
            target != null && menu.contains(target),
          )
          if !insideMenu then hideContextMenu()
        },
      )
  }

  def syncConnectionTool(panel: dom.Element): Unit = {
    clearConnectionStartClasses(panel)
    connectionToolStateVar.now().startElementId.foreach { startId =>
      findTaggedTypstElementById(panel, startId).foreach(
        _.classList.add("vistyp-connection-start"),
      )
    }
  }

  def syncInstanceSelection(panel: dom.Element): Unit = {
    clearSelectedInstanceClasses(panel)
    selectedInstanceId.foreach { instanceId =>
      findTaggedTypstElementById(panel, instanceId).foreach(
        _.classList.add("vistyp-selected-instance"),
      )
    }
  }

  private def selectInstance(instanceId: String): Unit = {
    if instances.exists(_.name == instanceId) then
      selectedInstanceId = Some(instanceId)
      refreshSelectedInstance()
      previewPanelElement.foreach(syncInstanceSelection)
  }

  private def showInstanceContextMenu(
      panel: dom.Element,
      evt: dom.MouseEvent,
  ): Unit = {
    val targetOpt =
      findTaggedTypstElement(evt.target.asInstanceOf[dom.Element])
    targetOpt match {
      case Some(target) =>
        evt.preventDefault()
        evt.stopPropagation()
        val instanceId = getTypstElementId(target)
        selectInstance(instanceId)
        showContextMenuForInstance(instanceId, evt.clientX, evt.clientY)
      case None =>
        hideContextMenu()
    }
  }

  private def showContextMenuForInstance(
      instanceId: String,
      clientX: Double,
      clientY: Double,
  ): Unit = {
    hideContextMenu()
    val menu = dom.document.createElement("div")
    menu.setAttribute("class", "vistyp-context-menu")
    menu.asInstanceOf[js.Dynamic].style.left = s"${clientX}px"
    menu.asInstanceOf[js.Dynamic].style.top = s"${clientY}px"

    menu.appendChild(contextMenuButton("Edit properties") { () =>
      selectInstance(instanceId)
      hideContextMenu()
    })
    menu.appendChild(contextMenuButton("Delete") { () =>
      deleteInstance(instanceId)
    })

    dom.document.body.appendChild(menu)
    contextMenuElement = Some(menu)
  }

  private def contextMenuButton(label: String)(action: () => Unit): dom.Element = {
    val button = dom.document.createElement("button")
    button.setAttribute("class", "vistyp-context-menu-item")
    button.textContent = label
    button.addEventListener(
      "click",
      (event: dom.Event) => {
        event.preventDefault()
        event.stopPropagation()
        action()
      },
    )
    button
  }

  private def hideContextMenu(): Unit = {
    contextMenuElement.foreach { menu =>
      if menu.parentNode != null then menu.parentNode.removeChild(menu)
    }
    contextMenuElement = None
  }

  private def schedulePreviewLayoutSync(panel: dom.Element): Unit = {
    dom.window.requestAnimationFrame { _ =>
      applyPreviewZoom(panel)
      syncGridMetrics(panel)
      syncConnectionTool(panel)
      syncInstanceSelection(panel)
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

  private def handleConnectionMouseDown(
      panel: dom.Element,
      evt: dom.MouseEvent,
  ): Unit = {
    evt.preventDefault()
    evt.stopPropagation()
    selectedElem = None
    dragStartPos = None

    val targetOpt =
      findTaggedTypstElement(evt.target.asInstanceOf[dom.Element])
    targetOpt match {
      case Some(target) =>
        val typstId = getTypstElementId(target)
        if !instances.exists(_.name == typstId) then return

        connectionToolStateVar.now().startElementId match {
          case Some(startId) if startId == typstId =>
            connectionToolStateVar.update(_.copy(startElementId = None))
            syncConnectionTool(panel)

          case Some(startId) =>
            connectionToolStateVar.update(_.copy(startElementId = None))
            insertConnection(startId, typstId)
            syncConnectionTool(panel)

          case None =>
            connectionToolStateVar.update(_.copy(startElementId = Some(typstId)))
            syncConnectionTool(panel)
        }

      case None =>
        connectionToolStateVar.update(_.copy(startElementId = None))
        syncConnectionTool(panel)
    }
  }

  def startDrag(panel: dom.Element, evt: dom.MouseEvent): Unit = {
    if evt.button != 0 then return
    hideContextMenu()

    if connectionToolStateVar.now().enabled then {
      handleConnectionMouseDown(panel, evt)
      return
    }

    val targetOpt =
      findTaggedTypstElement(evt.target.asInstanceOf[dom.Element]);

    val target = targetOpt match {
      case Some(target) => target
      case None =>
        selectedElem = None
        clearInstanceSelection()
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
    selectInstance(typstId)
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

  private def clearConnectionStartClasses(panel: dom.Element): Unit = {
    val elements = panel.querySelectorAll(".vistyp-connection-start")
    var idx = 0
    while idx < elements.length do
      elements(idx)
        .asInstanceOf[dom.Element]
        .classList
        .remove("vistyp-connection-start")
      idx += 1
  }

  private def clearSelectedInstanceClasses(panel: dom.Element): Unit = {
    val elements = panel.querySelectorAll(".vistyp-selected-instance")
    var idx = 0
    while idx < elements.length do
      elements(idx)
        .asInstanceOf[dom.Element]
        .classList
        .remove("vistyp-selected-instance")
      idx += 1
  }

  private def findTaggedTypstElementById(
      panel: dom.Element,
      typstId: String,
  ): Option[dom.Element] = {
    val elements = panel.querySelectorAll(".typst-cetz-elem")
    var idx = 0
    while idx < elements.length do
      val element = elements(idx).asInstanceOf[dom.Element]
      if getTypstElementId(element) == typstId then return Some(element)
      idx += 1

    None
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

case class ConnectionToolState(
    enabled: Boolean = false,
    startElementId: Option[String] = None,
)

case class SelectedInstanceDetails(
    id: String,
    name: String,
    ty: String,
    x: String,
    y: String,
    args: List[SelectedInstanceArg],
    isConnection: Boolean,
)

case class SelectedInstanceArg(
    name: String,
    kind: String,
    value: String,
    present: Boolean,
)
