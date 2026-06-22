package vistyp

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.DomApi

private enum ActivityView:
  case SourceCode, Package

trait Vistyp:
  val previewContentSignal: Signal[(String, Map[String, String])]
  val programContentVar: Var[String]
  val gridSettingsVar: Var[GridSettings]
  val assetLibraryStateSignal: Signal[AssetLibraryState]
  def previewSvgNow: String
  def generatedTypstSource: String

  def updateDefinition(code: String): Unit
  def updateDiagram(code: String): Unit
  def loadAssetIndex(url: String): Unit
  def insertResource(resourceId: String, name: String): Unit

  def onPreviewMounted(panel: dom.Element): Unit
  def applyPreviewZoom(panel: dom.Element): Unit
  def syncGridMetrics(panel: dom.Element): Unit
end Vistyp

class UI(vistyp: Vistyp):
  import vistyp.*;

  private val defaultLibraryUrl = "index.json"
  private val libraryUrlVar = Var(defaultLibraryUrl)
  private val activeActivityVar = Var(ActivityView.SourceCode)
  private val selectedResourceIdVar = Var(
    BuiltinAssets.resources.headOption.map(_.id).getOrElse(""),
  )
  private val insertNameVar = Var("")
  private val mainEditorVar = Var(Option.empty[MonacoEditor])
  private var keyboardShortcutsInstalled = false

  val builtinDefinitions = """#let x-circle(rad: 200, inner-text: "") = {
  circle((0, 0), radius: rad, name: node-label)
  debug-label((-rad*0.7, -rad*0.7))
  content(node-label, inner-text)
}
#let x-rect(x: 200, y: none, inner-text: "") = {
  let y = if y == none {
    x
  } else {
    y
  }
  rect((0, 0), (x, y), name: node-label)
  debug-label((0, 0))
  content(node-label, inner-text)
}
#let x-arrow(start: (0, 10), end: (50, 10), inner-text: "", mark: (end: ">")) = {
  set-style(mark: (fill: none, size: 14))
  line(start, end, name: "t", mark: mark)
  content("t", inner-text)
}
  """;

  def mainElement(): Element = {
    div(
      cls := "editor-main flex-column",
      onMountCallback(_ => {
        updateDefinition(builtinDefinitions)
        loadAssetIndex(defaultLibraryUrl)
        installKeyboardShortcuts()
      }),
      topPanel(),
      bottomPanel(),
    )
  }

  private def installKeyboardShortcuts(): Unit = {
    if keyboardShortcutsInstalled then return
    keyboardShortcutsInstalled = true
    dom.window.addEventListener(
      "keydown",
      event => handleGlobalKeydown(event.asInstanceOf[dom.KeyboardEvent]),
    )
  }

  private def handleGlobalKeydown(evt: dom.KeyboardEvent): Unit = {
    if isEditableShortcutTarget(evt.target) then return

    val usesModifier = evt.ctrlKey || evt.metaKey
    if !usesModifier then return

    evt.key.toLowerCase match {
      case "z" =>
        evt.preventDefault()
        runEditorAction(if evt.shiftKey then "redo" else "undo")
      case "y" =>
        evt.preventDefault()
        runEditorAction("redo")
      case _ =>
    }
  }

  private def isEditableShortcutTarget(target: dom.EventTarget | Null): Boolean = {
    if target == null then return false

    val element = target.asInstanceOf[js.Dynamic]
    val tagName = element.tagName.asInstanceOf[js.UndefOr[String]]
      .map(_.toLowerCase)
      .getOrElse("")
    val isContentEditable = element.isContentEditable
      .asInstanceOf[js.UndefOr[Boolean]]
      .getOrElse(false)
    val monacoElement =
      if js.isUndefined(element.closest) then null
      else element.closest(".monaco-editor")

    tagName == "input" ||
    tagName == "textarea" ||
    tagName == "select" ||
    isContentEditable ||
    monacoElement != null
  }

  def topPanel(): Element = {
    div(
      cls := "top-panel",
      div(
        cls := "top-panel-toolbar flex-row",
        img(
          cls := "app-logo",
          src := "favicon.svg",
        ),
        div(
          cls := "editor-menu-group",
          toolbarButton("Save", "Download the current source", saveSource()),
          toolbarButton("Load", "Open a local .typ source file", loadSource()),
        ),
        div(
          cls := "editor-menu-group",
          toolbarButton(
            "Undo",
            "Undo the latest source edit",
            runEditorAction("undo"),
            enabledSignal = mainEditorVar.signal.map(_.isDefined),
          ),
          toolbarButton(
            "Redo",
            "Redo the latest undone source edit",
            runEditorAction("redo"),
            enabledSignal = mainEditorVar.signal.map(_.isDefined),
          ),
        ),
        div(
          cls := "editor-menu-group",
          toolbarButton("SVG", "Export the current preview as SVG", exportSvg()),
          toolbarButton(
            "CeTZ",
            "Export the generated Typst/CeTZ source",
            exportCetz(),
          ),
          button(
            cls := "editor-menu editor-menu-disabled",
            disabled := true,
            title := "PDF export needs a PDF compile path in the Typst binding",
            "PDF",
          ),
        ),
        gridControls(),
      ),
    )
  }

  private def toolbarButton(
      label: String,
      tooltip: String,
      action: => Unit,
      enabledSignal: Signal[Boolean] = Val(true),
  ): Element =
    button(
      cls <-- enabledSignal.map(enabled =>
        if enabled then "editor-menu" else "editor-menu editor-menu-disabled",
      ),
      disabled <-- enabledSignal.map(!_),
      title := tooltip,
      label,
      onClick.mapTo(()) --> { _ =>
        action
      },
    )

  private def saveSource(): Unit =
    BrowserFiles.downloadText(
      "vistyp.typ",
      programContentVar.now(),
      "text/plain;charset=utf-8",
    )

  private def loadSource(): Unit =
    BrowserFiles.openTextFile(".typ,.txt,text/plain") { content =>
      programContentVar.set(content)
      if mainEditorVar.now().isEmpty then updateDiagram(content)
    }

  private def exportSvg(): Unit =
    BrowserFiles.downloadText(
      "vistyp.svg",
      previewSvgNow,
      "image/svg+xml;charset=utf-8",
    )

  private def exportCetz(): Unit =
    BrowserFiles.downloadText(
      "vistyp-cetz.typ",
      generatedTypstSource.trim + "\n",
      "text/plain;charset=utf-8",
    )

  private def runEditorAction(action: String): Unit =
    mainEditorVar.now().foreach { editor =>
      if activeActivityVar.now() == ActivityView.SourceCode then editor.focus()
      editor.trigger("vistyp.toolbar", action, null.asInstanceOf[js.Any])
    }

  def gridControls(): Element = {
    div(
      cls := "editor-action-group editor-grid-controls",
      label(
        cls := "editor-grid-toggle",
        input(
          typ := "checkbox",
          checked <-- gridSettingsVar.signal.map(_.enabled),
          onInput.mapToChecked --> { enabled =>
            gridSettingsVar.update(_.copy(enabled = enabled))
          },
        ),
        span("Grid"),
      ),
      input(
        cls := "editor-input-box editor-grid-size",
        typ := "number",
        value <-- gridSettingsVar.signal.map(settings =>
          Grid.clean(settings.size).toString,
        ),
        onInput.mapToValue --> { rawValue =>
          rawValue.toDoubleOption
            .filter(size => size > 0)
            .foreach(size =>
              gridSettingsVar.update(_.copy(size = Grid.clean(size))),
            )
        },
      ),
    )
  }

  def bottomPanel(): Element = {
    div(
      cls := "bottom-panel flex-row",
      activityBar(),
      div(
        cls := "activity-panel-shell",
        activityPanel(ActivityView.SourceCode, sourceCodePanel()),
        activityPanel(ActivityView.Package, packagePanel()),
      ),
      previewPanel(),
    )
  }

  private def activityPanel(view: ActivityView, content: Element): Element =
    div(
      cls <-- activeActivityVar.signal.map(active =>
        if active == view then "activity-panel-page is-active"
        else "activity-panel-page is-hidden",
      ),
      content,
    )

  def activityBar(): Element = {
    div(
      cls := "activity-bar",
      activityButton(ActivityView.SourceCode, "Source Code"),
      activityButton(ActivityView.Package, "Package"),
    )
  }

  private def activityButton(view: ActivityView, label: String): Element = {
    button(
      cls <-- activeActivityVar.signal.map(active =>
        if active == view then "activity-button is-active" else "activity-button",
      ),
      title := label,
      label,
      onClick.mapTo(view) --> activeActivityVar,
    )
  }

  def sourceCodePanel(): Element = {
    div(
      cls := "source-code-panel flex-column",
      div(cls := "activity-panel-title", "Source Code"),
      mainEditor(),
    )
  }

  def packagePanel(): Element = {
    div(
      cls := "package-panel flex-column",
      div(cls := "activity-panel-title", "Package"),
      div(
        cls := "package-load-row",
        input(
          cls := "editor-input-box editor-library-url",
          typ := "text",
          placeholder := "index.json URL",
          value <-- libraryUrlVar.signal,
          onInput.mapToValue --> libraryUrlVar,
        ),
        button(
          cls := "editor-input-button",
          "Load",
          onClick.mapTo(()) --> { _ =>
            loadAssetIndex(libraryUrlVar.now())
          },
        ),
      ),
      div(
        cls := "package-status",
        child.text <-- assetLibraryStateSignal.map(packageStatusText),
      ),
      div(
        cls := "package-list",
        children <-- assetLibraryStateSignal.map { state =>
          builtinPackageGroup() :: state.packages.map(packageGroup)
        },
      ),
    )
  }

  private def packageStatusText(state: AssetLibraryState): String =
    if state.loading then "loading"
    else
      state.error.getOrElse {
        if state.packages.isEmpty then "builtin"
        else s"${state.packages.length} package(s)"
      }

  private def builtinPackageGroup(): Element =
    packageResourceGroup("Builtin", "local", BuiltinAssets.resources)

  private def packageGroup(pkg: LoadedAssetPackage): Element =
    packageResourceGroup(pkg.key.name, pkg.key.id, pkg.resources)

  private def packageResourceGroup(
      name: String,
      meta: String,
      resources: List[AssetResource],
  ): Element = {
    div(
      cls := "package-group",
      div(
        cls := "package-group-header",
        div(cls := "package-group-name", name),
        div(cls := "package-group-meta", meta),
      ),
      div(
        cls := "package-resources",
        children <-- Val {
          if resources.isEmpty then List(div(cls := "package-empty", "No elements"))
          else resources.map(resourceButton)
        },
      ),
    )
  }

  private def resourceButton(resource: AssetResource): Element = {
    val args =
      if resource.args.isEmpty then "()"
      else resource.args.map(_.name).mkString("(", ", ", ")")
    button(
      cls := "package-resource",
      title := resource.id,
      onClick.mapTo(()) --> { _ =>
        selectedResourceIdVar.set(resource.id)
        insertResource(resource.id, "")
      },
      span(cls := "package-resource-name", resource.functionName),
      span(cls := "package-resource-args", args),
    )
  }

  //  $preview.bindElement(document.getElementById('preview-panel'), v => mainEditor.setValue(v));
  //  document.getElementById('export-svg').addEventListener('click', () => {
  //      $preview.doExport('svg');
  //  });
  //  document.getElementById('export-pdf').addEventListener('click', () => {
  //      $preview.doExport('pdf');
  //  });
  //  document.getElementById('export-cetz').addEventListener('click', () => {
  //      $preview.doExport('cetz');
  //  });
  //  document.getElementById('insert-elem').addEventListener('click', () => {
  //      let insertNameValue = insertName.value;
  //      if (!insertNameValue) {
  //          insertNameValue = 'node-' + Math.random().toString(36).substring(7).replace('0.', '');
  //      }
  //      console.log('insertSelector.value', insertSelector.value, insertNameValue);
  //      $preview.doInsertElem(insertSelector.value, insertNameValue);
  // });

  def definitionGroup(): Element = {
    div(
      cls := "editor-action-group",
      span("library:"),
      input(
        cls := "editor-input-box editor-library-url",
        typ := "text",
        placeholder := "index.json URL",
        value <-- libraryUrlVar.signal,
        onInput.mapToValue --> libraryUrlVar,
      ),
      button(
        cls := "editor-input-button",
        "Load",
        onClick.mapTo(()) --> { _ =>
          loadAssetIndex(libraryUrlVar.now())
        },
      ),
      span(
        cls := "editor-library-status",
        child.text <-- assetLibraryStateSignal.map { state =>
          if state.loading then "loading"
          else
            state.error.getOrElse {
              if state.packages.isEmpty then "builtin"
              else s"${state.packages.length} package(s)"
            }
        },
      ),
    )
  }

//         const checkDefinitionSource = () => {
//             if (definitionSourceSelector.value === 'builtin') {
//                 definitionEditor.setValue(builtinDefinitions);
//             } else if (definitionSourceSelector.value === 'url') {
//                 console.log('definitionSourceValue.value', definitionSourceValue.value);
//                 if (definitionSourceValue.value) {
//                     fetch(definitionSourceValue.value).then(r => r.text()).then(t => {
//                         definitionEditor.setValue(t);
//                     });
//                 }
//                 // set url param
//                 const urlParams = new URLSearchParams(window.location.search);
//                 urlParams.set('url', definitionSourceValue.value);
//                 window.history.replaceState({}, '', `${location.pathname}?${urlParams}`);
//             } else if (definitionSourceSelector.value === 'file') {
//                 const input = document.createElement('input');
//                 input.type = 'file';
//                 input.onchange = () => {
//                     const file = input.files[0];
//                     const reader = new FileReader();
//                     reader.onload = () => {
//                         definitionEditor.setValue(reader.result);
//                     };
//                     reader.readAsText(file);
//                     definitionSourceValue.value = file.name;
//                 };
//                 input.click();
//             }
//         };

//         /// check ?url=xxx option
//         const urlParams = new URLSearchParams(window.location.search);
//         const urlParamUrl = urlParams.get('url');
//         if (urlParamUrl) {
//             definitionSourceSelector.value = 'url';
//             definitionSourceValue.value = urlParamUrl;
//             checkDefinitionSource();
//         } else {
//             definitionEditor.setValue(builtinDefinitions);
//             mainEditor.setValue();
//         }

//         previewSelector.onchange = () => {
//             console.log('previewSelector.value', previewSelector.value);
//             $preview.doSelectDef(previewSelector.value);
//         };

//         $preview.doSelectDef('');

//         definitionSourceConfirm.onclick = checkDefinitionSource;
//         definitionSourceSelector.onchange = () => {
//             if (definitionSourceSelector.value === 'builtin' || definitionSourceSelector.value === 'file') {
//                 checkDefinitionSource();
//             }
//         };

  def elementGroup(): Element = {
    div(
      cls := "editor-action-group",
      span("element:"),
      select(
        cls := "editor-code-select editor-resource-select",
        value <-- selectedResourceIdVar.signal,
        onChange.mapToValue --> selectedResourceIdVar,
        children <-- assetLibraryStateSignal.map { state =>
          (BuiltinAssets.resources ++ state.packages.flatMap(_.resources)).map {
            resource =>
              option(
                value := resource.id,
                s"${resource.functionName} · ${resource.packageLabel}",
              )
          }
        },
      ),
      input(
        cls := "editor-input-box editor-insert-name",
        typ := "text",
        placeholder := "name",
        value <-- insertNameVar.signal,
        onInput.mapToValue --> insertNameVar,
      ),
      button(
        cls := "editor-input-button",
        "Insert",
        onClick.mapTo(()) --> { _ =>
          insertResource(selectedResourceIdVar.now(), insertNameVar.now())
          insertNameVar.set("")
        },
      ),
    )
  }

  def viewportGroup(): Element = {
    //   <div class="editor-action-group" style="float: right">
    //     <span>viewport:</span>
    //     <input
    //       id="input-svg-width"
    //       class="editor-input-box"
    //       type="text"
    //       placeholder="width"
    //       style="width: 2.5em"
    //     />
    //     <span>·</span>
    //     <input
    //       id="input-svg-height"
    //       class="editor-input-box"
    //       type="text"
    //       placeholder="height"
    //       style="width: 2.5em"
    //     />
    //   </div>
    div()
  }

  def defEditor(): Element = {
    div(
      cls := "def-editor",
      child.maybe <-- monacoLoadSignal.splitOption { (monaco, _) =>
        div(
          cls := "monaco-editor-host",
          onMountCallback(ctx => {
            dom.console.log("defEditor mounting", ctx.thisNode.ref);

            val definitionEditor = monaco.editor.create(
              ctx.thisNode.ref,
              js.Dynamic.literal(
                "value" -> "",
                "language" -> "typst",
                "theme" -> "tokyo-night",
                "semanticHighlighting.enabled" -> true,
                "bracketPairColorization.enabled" -> true,
              ),
            )
            definitionEditor.setValue(builtinDefinitions);
            updateDefinition(builtinDefinitions);
          }),
        )
      },
    )
  }

  def mainEditor(): Element = {
    div(
      cls := "main-editor",
      child.maybe <-- monacoLoadSignal.splitOption { (monaco, _) =>
        var mountedEditor = Option.empty[MonacoEditor]
        div(
          cls := "monaco-editor-host",
          onMountCallback(ctx => {
            dom.console.log("mainEditor mounting", ctx.thisNode.ref);
            val mainEditor = monaco.editor.create(
              ctx.thisNode.ref,
              js.Dynamic.literal(
                "value" -> "",
                "language" -> "typst",
                "theme" -> "tokyo-night",
                "semanticHighlighting.enabled" -> true,
                "bracketPairColorization.enabled" -> true,
              ),
            )
            mountedEditor = Some(mainEditor)
            mainEditorVar.set(Some(mainEditor))

            mainEditor
              .getModel()
              .onDidChangeContent((e: MonacoModelContentChange) => {
                if (!e.isFlush) {
                  val content = mainEditor.getValue()
                  programContentVar.set(content)
                  updateDiagram(content)
                }
              })

            var initialized = false
            def syncEditor(content: String): Unit = {
              if mainEditor.getValue() != content then {
                if !initialized then
                  mainEditor.setValue(content)
                  updateDiagram(content)
                else
                  mainEditor.pushUndoStop()
                  mainEditor.executeEdits(
                    "vistyp.state",
                    js.Array(
                      js.Dynamic
                        .literal(
                          range = mainEditor.getModel().getFullModelRange(),
                          text = content,
                        )
                        .asInstanceOf[js.Object],
                    ),
                  )
                  mainEditor.pushUndoStop()
              }

              if !initialized then
                initialized = true
            }

            def syncEditorWithSetValue(content: String): Unit = {
              if mainEditor.getValue() != content then
                mainEditor.setValue(content)
                updateDiagram(content)
              initialized = true
            }

            programContentVar.signal.foreach(syncEditor)(using ctx.owner)
            activeActivityVar.signal.foreach { active =>
              if active == ActivityView.SourceCode then
                dom.window.setTimeout(() => mainEditor.layout(), 0)
            }(using ctx.owner)
            syncEditorWithSetValue(programContentVar.now())

          }),
          onUnmountCallback(_ => {
            mountedEditor.foreach { editor =>
              if mainEditorVar.now().contains(editor) then mainEditorVar.set(None)
              editor.dispose()
            }
            mountedEditor = None
          }),
        )
      },
    )
  }

  def previewPanel(): Element = {
    import instrument.processSvg
    div(
      cls := "preview",
      div(
        cls <-- gridSettingsVar.signal.map(settings =>
          if Grid.active(settings) then "preview-panel preview-panel-grid"
          else "preview-panel",
        ),
        styleAttr <-- gridSettingsVar.signal.map(settings =>
          s"--vistyp-grid-size: ${Grid.clean(settings.size)}px",
        ),
        onMountCallback(ctx => {
          dom.console.log("previewPanel mounting", ctx.thisNode.ref);
          vistyp.onPreviewMounted(ctx.thisNode.ref);
          previewContentSignal.foreach(_ =>
            schedulePreviewLayoutSync(ctx.thisNode.ref),
          )(using ctx.owner)
          gridSettingsVar.signal.foreach(_ =>
            schedulePreviewLayoutSync(ctx.thisNode.ref),
          )(using ctx.owner)
          ctx.thisNode.ref.addEventListener(
            "scroll",
            _ => schedulePreviewLayoutSync(ctx.thisNode.ref),
          )
        }),
        child <-- previewContentSignal.map { case (content, mapping) =>
          val rawSvg = DomApi.unsafeParseSvgString(content)
          foreignSvgElement(processSvg(rawSvg, mapping))
        },
      ),
    )
  }

  private def schedulePreviewLayoutSync(panel: dom.Element): Unit = {
    dom.window.setTimeout(
      () => {
        vistyp.applyPreviewZoom(panel)
        vistyp.syncGridMetrics(panel)
      },
      0,
    )
  }

end UI

private object BrowserFiles:
  def downloadText(filename: String, content: String, mimeType: String): Unit = {
    val blobOptions = js.Dynamic.literal()
    blobOptions.updateDynamic("type")(mimeType)
    val blob = js.Dynamic.newInstance(js.Dynamic.global.Blob)(
      js.Array(content),
      blobOptions,
    )
    val urlApi = js.Dynamic.global.URL
    val url = urlApi.createObjectURL(blob).asInstanceOf[String]
    val anchor = dom.document.createElement("a")
    val anchorDynamic = anchor.asInstanceOf[js.Dynamic]

    anchorDynamic.href = url
    anchorDynamic.download = filename
    anchorDynamic.style.display = "none"
    dom.document.body.appendChild(anchor)
    anchorDynamic.click()
    removeElement(anchor)
    dom.window.setTimeout(() => urlApi.revokeObjectURL(url), 0)
  }

  def openTextFile(accept: String)(onLoaded: String => Unit): Unit = {
    val input = dom.document.createElement("input")
    val inputDynamic = input.asInstanceOf[js.Dynamic]

    inputDynamic.updateDynamic("type")("file")
    inputDynamic.accept = accept
    inputDynamic.style.display = "none"
    input.addEventListener(
      "change",
      (_: dom.Event) => {
        val files = inputDynamic.files
        if (
          files != null &&
          !js.isUndefined(files.asInstanceOf[js.Any]) &&
          files.length.asInstanceOf[Int] > 0
        ) {
          val file = files.item(0)
          file
            .text()
            .asInstanceOf[js.Promise[String]]
            .`then`((content: String) => {
              onLoaded(content)
            })
            .`catch`((error: Any) => {
              dom.console.error("Failed to load source file", error)
            })
        }

        removeElement(input)
      },
    )

    dom.document.body.appendChild(input)
    inputDynamic.click()
  }

  private def removeElement(element: dom.Element): Unit =
    if element.parentNode != null then element.parentNode.removeChild(element)
