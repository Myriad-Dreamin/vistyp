package vistyp

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.DomApi

trait Vistyp:
  val previewContentSignal: Signal[(String, Map[String, String])]
  val programContentVar: Var[String]

  def updateDefinition(code: String): Unit
  def updateDiagram(code: String): Unit

  def onPreviewMounted(panel: dom.Element): Unit
end Vistyp

class UI(vistyp: Vistyp):
  import vistyp.*;

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
      styleAttr(
        "height: 100vh",
      ),
      topPanel(),
      bottomPanel(),
    )
  }

  def topPanel(): Element = {
    div(
      cls := "top-panel",
      styleAttr(
        "flex: 0 0 auto",
      ),
      div(
        cls := "flex-row",
        styleAttr(
          "margin: 5px; align-items: center",
        ),
        img(
          src := "favicon.svg",
          styleAttr(
            "height: 1rem; margin: 3px; margin-left: 2px",
          ),
        ),
        div(
          cls := "editor-menu-group",
          styleAttr(
            "margin-left: 1em; padding: 1px 3px",
          ),
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
      ),
    )
  }

  def bottomPanel(): Element = {
    div(
      cls := "bottom-panel flex-row",
      styleAttr("flex: 1"),
      div(
        styleAttr("flex: 0 0 35px"),
      ),
      div(
        cls := "flex-column",
        styleAttr("flex: 1"),
        div(
          styleAttr("margin: 5px; flex: 1"),
          definitionGroup(),
          elementGroup(),
          viewportGroup(),
        ),
        div(
          cls := "flex-row",
          styleAttr("flex: 1"),
          div(
            cls := "flex-column",
            styleAttr("flex: 4; height: 100vh"),
            defEditor(),
            mainEditor(),
          ),
          previewPanel(),
        ),
      ),
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
    // <div class="editor-action-group">
    // <span>definition:</span>
    // <select id="preview-selector" class="editor-code-select"></select>
    // <span>from</span>
    // <div
    //     class="flex-row"
    //     style="
    //     display: inline-flex;
    //     justify-content: space-around;
    //     gap: 5px;
    //     "
    // >
    //     <select
    //     id="definition-source-selector"
    //     class="editor-code-select"
    //     style="width: 5em"
    //     >
    //     <option value="builtin">Builtin</option>
    //     <option value="url">Url</option>
    //     <option value="file">File</option>
    //     </select>
    //     <input
    //     id="definition-source-value"
    //     class="editor-input-box"
    //     type="text"
    //     placeholder="https://..."
    //     />
    //     <button
    //     id="definition-source-confirm"
    //     class="editor-input-button"
    //     >
    //     Pull
    //     </button>
    // </div>
    // </div>
    div()
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
    //  <div class="editor-action-group">
    // <span>element:</span>
    // <div
    //     class="flex-row"
    //     style="
    //     display: inline-flex;
    //     justify-content: space-around;
    //     gap: 5px;
    //     "
    // >
    //     <select id="insert-selector" class="editor-code-select"></select>
    //     <input
    //     id="insert-name"
    //     class="editor-input-box"
    //     type="text"
    //     placeholder="name"
    //     />
    //     <button id="insert-elem" class="editor-input-button">
    //     Insert
    //     </button>
    // </div>
    // </div>
    div()
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
      styleAttr("flex: 5"),
      child.maybe <-- monacoLoadSignal.splitOption { (monaco, _) =>
        div(
          styleAttr(
            "width: 100%; height: 100%",
          ),
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
      styleAttr("flex: 2"),
      child.maybe <-- monacoLoadSignal.splitOption { (monaco, _) =>
        div(
          styleAttr(
            "width: 100%; height: 100%",
          ),
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

            mainEditor
              .getModel()
              .onDidChangeContent((e: MonacoModelContentChange) => {
                if (!e.isFlush) {
                  updateDiagram(mainEditor.getValue());
                }
              })

            programContentVar.signal
              .foreach(content => {
                mainEditor.setValue(content);
                updateDiagram(content);
              })(ctx.owner)
            updateDiagram(programContentVar.now());

          }),
        )
      },
    )
  }

  def previewPanel(): Element = {
    import instrument.processSvg
    div(
      cls := "preview",
      styleAttr(
        "flex: 6",
      ),
      div(
        cls := "preview-panel",
        onMountCallback(ctx => {
          dom.console.log("previewPanel mounting", ctx.thisNode.ref);
          vistyp.onPreviewMounted(ctx.thisNode.ref);
        }),
        child <-- previewContentSignal.map(state => {
          val (content, mapping) = state
          val rawSvg = DomApi.unsafeParseSvgString(content)
          foreignSvgElement(processSvg(rawSvg, mapping))
        }),
      ),
    )
  }

end UI
