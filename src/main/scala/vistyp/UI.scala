package vistyp

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.DomApi

private enum ActivityView:
  case SourceCode, Package

private case class PreviewContextMenuState(
    elementId: String,
    x: Double,
    y: Double,
)

trait Vistyp:
  val previewContentSignal: Signal[(String, Map[String, String])]
  val programContentVar: Var[String]
  val gridSettingsVar: Var[GridSettings]
  val assetLibraryStateSignal: Signal[AssetLibraryState]

  def updateDefinition(code: String): Unit
  def updateDiagram(code: String): Unit
  def loadAssetIndex(url: String): Unit
  def insertResource(resourceId: String, name: String): Unit
  def elementIdAt(target: dom.EventTarget): Option[String]
  def deleteElement(elementId: String): Unit
  def elementProperties(elementId: String): Option[ElementPropertyState]
  def saveElementProperties(
      state: ElementPropertyState,
  ): Either[String, ElementPropertyState]
  def saveElementSource(
      elementId: String,
      sourceCode: String,
  ): Either[String, ElementPropertyState]

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
  private val previewContextMenuVar = Var(Option.empty[PreviewContextMenuState])
  private val elementPropertyVar = Var(Option.empty[ElementPropertyState])

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
        updateDiagram(programContentVar.now())
        loadAssetIndex(defaultLibraryUrl)
      }),
      topPanel(),
      bottomPanel(),
    )
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
          a(
            cls := "editor-menu export-svg",
            "SVG",
          ),
          a(
            cls := "editor-menu export-pdf",
            "PDF",
          ),
          a(
            cls := "editor-menu export-cetz",
            "CeTZ",
          ),
          a(
            cls := "editor-menu export-elem",
            "Element",
          ),
        ),
        gridControls(),
      ),
    )
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
        child <-- activeActivityVar.signal.map {
          case ActivityView.SourceCode => sourceCodePanel()
          case ActivityView.Package    => packagePanel()
        },
      ),
      previewPanel(),
    )
  }

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
      selectedElementSourcePanel(),
      selectedElementPropertiesPanel(),
    )
  }

  private def selectedElementSourcePanel(): Element = {
    div(
      cls := "element-source-pane flex-column",
      div(cls := "element-pane-title", "Element Source"),
      child.maybe <-- elementPropertyVar.signal.splitOption {
        (initialState, stateSignal) =>
          elementSourceEditor(initialState, stateSignal)
      },
      child.maybe <-- elementPropertyVar.signal.map {
        case Some(_) => None
        case None =>
          Some(div(cls := "element-inspector-empty", "No element selected"))
      },
    )
  }

  private def elementSourceEditor(
      initialState: ElementPropertyState,
      stateSignal: Signal[ElementPropertyState],
  ): Element = {
    div(
      cls := "element-source-editor-shell flex-column",
      div(
        cls := "element-source-editor",
        child.maybe <-- monacoLoadSignal.splitOption { (monaco, _) =>
          div(
            cls := "monaco-editor-host",
            onMountCallback(ctx => {
              val sourceEditor = monaco.editor.create(
                ctx.thisNode.ref,
                js.Dynamic.literal(
                  "value" -> initialState.sourceCode,
                  "language" -> "typst",
                  "theme" -> "tokyo-night",
                  "semanticHighlighting.enabled" -> true,
                  "bracketPairColorization.enabled" -> true,
                  "minimap" -> js.Dynamic.literal("enabled" -> false),
                  "scrollBeyondLastLine" -> false,
                  "automaticLayout" -> true,
                ),
              )

              sourceEditor
                .getModel()
                .onDidChangeContent((e: MonacoModelContentChange) => {
                  if (!e.isFlush) {
                    val sourceCode = sourceEditor.getValue()
                    syncElementSource(sourceCode)
                  }
                })

              stateSignal.map(_.sourceCode).foreach { sourceCode =>
                if sourceEditor.getValue() != sourceCode then
                  sourceEditor.setValue(sourceCode)
              }(using ctx.owner)
            }),
          )
        },
      ),
    )
  }

  private def selectedElementPropertiesPanel(): Element = {
    div(
      cls := "element-properties-pane flex-column",
      child.maybe <-- elementPropertyVar.signal.splitOption {
        (_, stateSignal) =>
          elementPropertiesForm(stateSignal)
      },
      child.maybe <-- elementPropertyVar.signal.map {
        case Some(_) => None
        case None =>
          Some(div(cls := "element-inspector-empty", "No properties"))
      },
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
                "minimap" -> js.Dynamic.literal("enabled" -> false),
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
                "minimap" -> js.Dynamic.literal("enabled" -> false),
              ),
            )

            mainEditor
              .getModel()
              .onDidChangeContent((e: MonacoModelContentChange) => {
                if (!e.isFlush) {
                  val content = mainEditor.getValue()
                  programContentVar.set(content)
                  updateDiagram(content)
                }
              })

            def syncEditor(content: String): Unit = {
              if mainEditor.getValue() != content then
                mainEditor.setValue(content)
                updateDiagram(content)
            }

            programContentVar.signal.foreach(syncEditor)(using ctx.owner)
            syncEditor(programContentVar.now())

          }),
        )
      },
    )
  }

  def previewPanel(): Element = {
    import instrument.processSvg
    div(
      cls := "preview",
      onClick --> { _ =>
        previewContextMenuVar.set(None)
      },
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
          ctx.thisNode.ref.addEventListener(
            "mousedown",
            e => handlePreviewMouseDown(e.asInstanceOf[dom.MouseEvent]),
          )
          ctx.thisNode.ref.addEventListener(
            "click",
            e => handlePreviewClick(e.asInstanceOf[dom.MouseEvent]),
          )
          ctx.thisNode.ref.addEventListener(
            "contextmenu",
            e => handlePreviewContextMenu(e.asInstanceOf[dom.MouseEvent]),
          )
        }),
        child <-- previewContentSignal.map { case (content, mapping) =>
          val rawSvg = DomApi.unsafeParseSvgString(content)
          foreignSvgElement(processSvg(rawSvg, mapping))
        },
      ),
      child.maybe <-- previewContextMenuVar.signal.map(_.map(contextMenu)),
    )
  }

  private def handlePreviewMouseDown(evt: dom.MouseEvent): Unit = {
    if evt.button != 0 then return

    elementIdAt(evt.target).foreach { elementId =>
      selectElement(elementId)
    }
  }

  private def handlePreviewClick(evt: dom.MouseEvent): Unit = {
    if evt.button != 0 then return

    elementIdAt(evt.target).foreach { elementId =>
      selectElement(elementId)
    }
  }

  private def handlePreviewContextMenu(evt: dom.MouseEvent): Unit = {
    elementIdAt(evt.target) match
      case Some(elementId) =>
        evt.preventDefault()
        evt.stopPropagation()
        previewContextMenuVar.set(
          Some(PreviewContextMenuState(elementId, evt.clientX, evt.clientY)),
        )
      case None =>
        previewContextMenuVar.set(None)
  }

  private def contextMenu(menu: PreviewContextMenuState): Element = {
    div(
      cls := "preview-context-menu",
      styleAttr := s"left: ${menu.x}px; top: ${menu.y}px;",
      button(
        typ := "button",
        cls := "preview-context-menu-item",
        "Properties",
        onClick --> { evt =>
          evt.stopPropagation()
          previewContextMenuVar.set(None)
          selectElement(menu.elementId)
        },
      ),
      button(
        typ := "button",
        cls := "preview-context-menu-item preview-context-menu-danger",
        "Delete element",
        onClick --> { evt =>
          evt.stopPropagation()
          previewContextMenuVar.set(None)
          elementPropertyVar.update {
            case Some(state) if state.elementId == menu.elementId => None
            case other                                           => other
          }
          deleteElement(menu.elementId)
        },
      ),
    )
  }

  private def selectElement(elementId: String): Unit = {
    previewContextMenuVar.set(None)
    elementPropertyVar.set(elementProperties(elementId))
    activeActivityVar.set(ActivityView.SourceCode)
  }

  private def elementPropertiesForm(
      stateSignal: Signal[ElementPropertyState],
  ): Element = {
    div(
      cls := "element-properties-form flex-column",
      div(
        cls := "element-properties-header",
        div(
          div(cls := "element-properties-title", "Element Properties"),
          div(
            cls := "element-properties-subtitle",
            child.text <-- stateSignal.map(state =>
              s"${state.elementId} · ${state.resourceLabel}",
            ),
          ),
        ),
      ),
      div(
        cls := "element-properties-body",
        div(
          cls := "element-properties-section",
          div(cls := "element-properties-section-title", "Position"),
          div(
            cls := "element-properties-grid two-columns",
            propertyTextInput(
              "X",
              stateSignal.map(_.positionX),
              value =>
                syncElementProperties(
                  _.copy(positionX = value, error = None),
                ),
            ),
            propertyTextInput(
              "Y",
              stateSignal.map(_.positionY),
              value =>
                syncElementProperties(
                  _.copy(positionY = value, error = None),
                ),
            ),
          ),
        ),
        div(
          cls := "element-properties-section",
          div(cls := "element-properties-section-title", "Properties"),
          div(
            cls := "element-properties-grid",
            children <-- stateSignal.map(_.fields).split(_.name) {
              (name, _, fieldSignal) =>
                propertyFieldInput(name, fieldSignal)
            },
          ),
        ),
        div(
          cls := "element-properties-section",
          div(cls := "element-properties-section-title", "Extra arguments"),
          label(
            cls := "element-property-field",
            textArea(
              cls := "element-property-input element-property-textarea",
              value <-- stateSignal.map(_.extraArgs),
              onInput.mapToValue --> { value =>
                syncElementProperties(
                  _.copy(extraArgs = value, error = None),
                )
              },
            ),
          ),
        ),
        child.maybe <-- stateSignal.map(
          _.error.map(error => div(cls := "element-properties-error", error)),
        ),
      ),
    )
  }

  private def propertyFieldInput(
      name: String,
      fieldSignal: Signal[ElementPropertyField],
  ): Element = {
    label(
      cls := "element-property-field",
      span(cls := "element-property-label", name),
      input(
        cls <-- fieldSignal.map { field =>
          if field.kind != "text" || !field.textLiteral then
            "element-property-input is-code"
          else "element-property-input"
        },
        typ := "text",
        value <-- fieldSignal.map(_.value),
        onInput.mapToValue --> { value =>
          syncElementPropertyField(name, value)
        },
      ),
    )
  }

  private def propertyTextInput(
      labelText: String,
      valueSignal: Signal[String],
      updateValue: String => Unit,
      code: Boolean = true,
  ): Element = {
    label(
      cls := "element-property-field",
      span(cls := "element-property-label", labelText),
      input(
        cls := {
          if code then "element-property-input is-code"
          else "element-property-input"
        },
        typ := "text",
        value <-- valueSignal,
        onInput.mapToValue --> updateValue,
      ),
    )
  }

  private def syncElementProperties(
      update: ElementPropertyState => ElementPropertyState,
  ): Unit =
    elementPropertyVar.now().foreach { state =>
      val nextState = update(state)
      saveElementProperties(nextState) match
        case Right(updatedState) =>
          elementPropertyVar.set(Some(updatedState))
        case Left(error) =>
          elementPropertyVar.set(Some(nextState.copy(error = Some(error))))
    }

  private def syncElementPropertyField(name: String, value: String): Unit =
    syncElementProperties { state =>
      state.copy(
        fields = state.fields.map { field =>
          if field.name == name then field.copy(value = value)
          else field
        },
        error = None,
      )
    }

  private def syncElementSource(sourceCode: String): Unit =
    elementPropertyVar.now().foreach { state =>
      val nextState = state.copy(sourceCode = sourceCode, error = None)
      saveElementSource(state.elementId, sourceCode) match
        case Right(updatedState) =>
          elementPropertyVar.set(Some(updatedState.copy(sourceCode = sourceCode)))
        case Left(error) =>
          elementPropertyVar.set(Some(nextState.copy(error = Some(error))))
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
