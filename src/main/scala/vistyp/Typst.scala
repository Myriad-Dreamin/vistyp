package vistyp.binding

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.dom

@js.native
@JSImport("@myriaddreamin/typst.ts/dist/esm/contrib/snippet.mjs", "$typst")
private object TypstTs extends js.Object {

  def setCompilerInitOptions(options: js.Object): Unit = js.native
  def setRendererInitOptions(options: js.Object): Unit = js.native

  def addSource(path: String, content: String): js.Promise[Unit] = js.native

  def svg(options: js.Object): js.Promise[String] = js.native

  def getSemanticTokenLegend(): js.Promise[js.Object] = js.native
  def getSemanticTokens(options: js.Object): js.Promise[js.Any] = js.native
}

object Typst:
  export TypstTs.*

  // /// Begin of Retrieve Wasm Modules from somewhere
  // /// We need a compiler module and a renderer module
  // /// - `@myriaddreamin/typst-ts-web-compiler`
  // /// - `@myriaddreamin/typst-ts-renderer`
  def configureWasm() = {
    dom.console.log("TypstTs", TypstTs)

    // Bundle
    // @ts-ignore
    // import compiler from '@myriaddreamin/typst-ts-web-compiler/pkg/typst_ts_web_compiler_bg.wasm?url';
    // @ts-ignore
    // import renderer from '@myriaddreamin/typst-ts-renderer/pkg/typst_ts_renderer_bg.wasm?url';

    // type ModuleSource = 'local' | 'jsdelivr';
    val moduleSource: String = dom.window
      .asInstanceOf[js.Dynamic]
      .$typst$moduleSource
      .asInstanceOf[js.UndefOr[String]]
      .getOrElse("jsdelivr")

    var compiler: js.Dynamic = js.Dynamic.literal()
    var renderer: js.Dynamic = js.Dynamic.literal()

    def fetch(url: String): js.Dynamic = {
      dom.window.asInstanceOf[js.Dynamic].fetch(url)
    }

    dom.console.log("moduleSource", moduleSource)
    moduleSource match {
      case "jsdelivr" =>
        compiler = fetch(
          "https://cdn.jsdelivr.net/npm/@myriaddreamin/typst-ts-web-compiler@0.5.4/pkg/typst_ts_web_compiler_bg.wasm",
        )
        renderer = fetch(
          "https://cdn.jsdelivr.net/npm/@myriaddreamin/typst-ts-renderer@0.5.4/pkg/typst_ts_renderer_bg.wasm",
        )
      case "local" =>
        compiler = fetch(
          "http://127.0.0.1:20810/base/node_modules/@myriaddreamin/typst-ts-web-compiler/pkg/typst_ts_web_compiler_bg.wasm",
        );
        renderer = fetch(
          "http://127.0.0.1:20810/base/node_modules/@myriaddreamin/typst-ts-renderer/pkg/typst_ts_renderer_bg.wasm",
        );
      case _ =>
        dom.console.warn("unknown module source for importing typst module")
    }

    Typst.setCompilerInitOptions(
      js.Dynamic.literal(
        getModule = () =>
          compiler || dom.window.asInstanceOf[js.Dynamic].$wasm$typst_compiler,
      ),
    )

    Typst.setRendererInitOptions(
      js.Dynamic.literal(
        getModule = () =>
          renderer || dom.window.asInstanceOf[js.Dynamic].$wasm$typst_renderer,
      ),
    )
  }

  def previewSvg(mainContent: String): js.Promise[String] = {
    addSource("/preview.typ", mainContent)
      .`then`(_ => svg(js.Dynamic.literal(mainFilePath = "/preview.typ")))
      .asInstanceOf[js.Promise[String]]
  }
  // await $typst.addSource('/preview.typ', mainContent);
  // return $typst.svg({ mainFilePath: '/preview.typ' });

// const insertSelector = document.getElementById('insert-selector');
// const insertName = document.getElementById('insert-name');
// const previewSelector = document.getElementById('preview-selector');
// const definitionSourceSelector = document.getElementById('definition-source-selector');
// const definitionSourceValue = document.getElementById('definition-source-value');
// const definitionSourceConfirm = document.getElementById('definition-source-confirm');
// const inputSvgWidth = document.getElementById('input-svg-width');
// const inputSvgHeight = document.getElementById('input-svg-height');
// inputSvgWidth.value = inputSvgHeight.value = '600';

//         const checkLength = (v) => {
//             if (v.endsWith('pt')) {
//                 v = v.substring(0, t.length - 2);
//             }
//             if (!v) {
//                 v = '600';
//             }

//             const t = Number.parseFloat(v);
//             if (Number.isNaN(t)) {
//                 throw new Error('invalid number');
//             }

//             return `${t}pt`;
//         };

//         const triggerSyncEditorState = () => {
//             const defs = {
//                 content: definitionEditor.getValue(),
//                 width: checkLength(inputSvgWidth.value),
//                 height: checkLength(inputSvgHeight.value),
//             };
//             $preview.doSetDefinitions(defs);
//             previewSelector.innerHTML = '';
//             insertSelector.innerHTML = '';
//             for (const def of ['main', ...$preview.getDefinitionNames()]) {
//                 const option = document.createElement('option');
//                 option.value = def;
//                 option.innerText = def;
//                 previewSelector.appendChild(option.cloneNode(true));
//                 if (def !== 'main') {
//                     insertSelector.appendChild(option);
//                 }
//             }
//         };
//         definitionEditor.getModel().onDidChangeContent(e => {
//             triggerSyncEditorState();
//         });
//         inputSvgWidth.oninput = triggerSyncEditorState;
//         inputSvgHeight.oninput = triggerSyncEditorState;

//         const triggerSyncMain = () => {
//             try {
//                 $preview.doSetMainContent({
//                     content: mainEditor.getValue(),
//                 });
//             } catch (e) {
//                 console.log('error', e);
//             }
//         };
//         mainEditor.getModel().onDidChangeContent(e => {
//             if (e.isFlush) {
//                 return;
//             }
//             triggerSyncMain();
//         });
end Typst
