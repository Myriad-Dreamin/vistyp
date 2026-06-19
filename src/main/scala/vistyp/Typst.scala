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
  def getCompiler(): js.Promise[js.Dynamic] = js.native
  def getRenderer(): js.Promise[js.Dynamic] = js.native

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
          "https://cdn.jsdelivr.net/npm/@myriaddreamin/typst-ts-web-compiler@0.8.0-rc1/pkg/typst_ts_web_compiler_bg.wasm",
        )
        renderer = fetch(
          "https://cdn.jsdelivr.net/npm/@myriaddreamin/typst-ts-renderer@0.8.0-rc1/pkg/typst_ts_renderer_bg.wasm",
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
      .`then`(_ => compileSvgWithDiagnostics("/preview.typ"))
      .`catch`((error: Any) => diagnosticSvg(formatDiagnosticError(error)))
      .asInstanceOf[js.Promise[String]]
  }

  private def compileSvgWithDiagnostics(
      mainFilePath: String,
  ): js.Promise[String] = {
    new js.Promise[String]((resolve, _) => {
      getCompiler()
        .`then`((compiler: js.Dynamic) => {
          compiler
            .reset()
            .asInstanceOf[js.Promise[Unit]]
            .`then`(_ => {
              val compilePromise = compiler
                .compile(
                  js.Dynamic.literal(
                    mainFilePath = mainFilePath,
                    diagnostics = "full",
                  ),
                )
                .asInstanceOf[js.Promise[js.Dynamic]]

              compilePromise
                .`then`((compileResult: js.Dynamic) => {
                  formatDiagnostics(compileResult.diagnostics) match {
                    case Some(diagnostics) => resolve(diagnosticSvg(diagnostics))
                    case None =>
                      compileResult.result
                        .asInstanceOf[js.UndefOr[js.Any]]
                        .toOption match {
                        case Some(vectorData) =>
                          renderSvg(vectorData)
                            .`then`((svg: String) => resolve(svg))
                            .`catch`((error: Any) =>
                              resolve(diagnosticSvg(formatDiagnosticError(error))),
                            )
                        case None =>
                          resolve(
                            diagnosticSvg(
                              "Typst returned neither an artifact nor diagnostics.",
                            ),
                          )
                      }
                  }
                })
                .`catch`((error: Any) =>
                  resolve(diagnosticSvg(formatDiagnosticError(error))),
                )
            })
            .`catch`((error: Any) =>
              resolve(diagnosticSvg(formatDiagnosticError(error))),
            )
        })
        .`catch`((error: Any) =>
          resolve(diagnosticSvg(formatDiagnosticError(error))),
        )
    })
  }

  private def renderSvg(vectorData: js.Any): js.Promise[String] = {
    getRenderer()
      .`then`((renderer: js.Dynamic) =>
        renderer
          .runWithSession((session: js.Dynamic) => {
            renderer.manipulateData(
              js.Dynamic.literal(
                renderSession = session,
                action = "reset",
                data = vectorData,
              ),
            )
            renderer
              .renderSvg(js.Dynamic.literal(renderSession = session))
              .asInstanceOf[js.Promise[String]]
          })
          .asInstanceOf[js.Promise[String]],
      )
      .asInstanceOf[js.Promise[String]]
  }

  private def formatDiagnostics(value: js.Dynamic): Option[String] = {
    asArray(value).filter(_.length > 0).map { diagnostics =>
      (0 until diagnostics.length)
        .map(i => formatDiagnosticItem(diagnostics(i)))
        .mkString("\n\n")
    }
  }

  private def formatDiagnosticError(error: Any): String = {
    val value = error.asInstanceOf[js.Any]
    asArray(value) match {
      case Some(diagnostics) if diagnostics.length > 0 =>
        (0 until diagnostics.length)
          .map(i => formatDiagnosticItem(diagnostics(i)))
          .mkString("\n\n")
      case _ => formatDiagnosticItem(value)
    }
  }

  private def formatDiagnosticItem(value: js.Any): String = {
    if (js.isUndefined(value) || value == null) {
      "empty diagnostic"
    } else if (js.typeOf(value) == "string") {
      value.asInstanceOf[String]
    } else if (js.typeOf(value) != "object" && js.typeOf(value) != "function") {
      jsValueToString(value)
    } else {
      val diagnostic = value.asInstanceOf[js.Dynamic]
      val severity = optionalValue(diagnostic.severity)
        .map(_.toLowerCase)
        .getOrElse("error")
      val message = optionalValue(diagnostic.message)
        .getOrElse(jsValueToString(value))
      val builder = new StringBuilder()

      formatDiagnosticLocation(diagnostic) match {
        case Some(location) =>
          builder.append(location).append(": ")
        case None =>
      }
      builder.append(severity).append(": ").append(message)

      optionalArray(diagnostic.trace).foreach { trace =>
        val frames = (0 until trace.length)
          .map(i => jsValueToString(trace(i)))
          .filter(_.nonEmpty)
        if (frames.nonEmpty) {
          builder.append("\ntrace: ").append(frames.mkString(" -> "))
        }
      }

      optionalArray(diagnostic.hints).foreach { hints =>
        val lines = (0 until hints.length)
          .map(i => jsValueToString(hints(i)))
          .filter(_.nonEmpty)
        if (lines.nonEmpty) {
          builder.append("\nhints:\n")
          lines.foreach(line => builder.append("- ").append(line).append("\n"))
        }
      }

      builder.toString().trim()
    }
  }

  private def formatDiagnosticLocation(diagnostic: js.Dynamic): Option[String] = {
    val path = optionalValue(diagnostic.path)
    val range = optionalValue(diagnostic.range)
    val pkg = optionalValue(diagnostic.`package`)
      .filter(_.nonEmpty)
      .map(_ + "@")
      .getOrElse("")

    path match {
      case Some(pathValue) if pathValue.nonEmpty && range.exists(_.nonEmpty) =>
        Some(pkg + pathValue + ":" + range.get)
      case Some(pathValue) if pathValue.nonEmpty =>
        Some(pkg + pathValue)
      case _ =>
        optionalValue(diagnostic.span).filter(_.nonEmpty).map("span: " + _)
    }
  }

  private def optionalValue(value: js.Dynamic): Option[String] = {
    val raw = value.asInstanceOf[js.Any]
    if (js.isUndefined(raw) || raw == null) None
    else Some(jsValueToString(raw))
  }

  private def optionalArray(value: js.Dynamic): Option[js.Array[js.Any]] = {
    asArray(value.asInstanceOf[js.Any])
  }

  private def asArray(value: js.Any): Option[js.Array[js.Any]] = {
    if (js.isUndefined(value) || value == null || !js.Array.isArray(value)) {
      None
    } else {
      Some(value.asInstanceOf[js.Array[js.Any]])
    }
  }

  private def jsValueToString(value: js.Any): String = {
    js.Dynamic.global.String(value).asInstanceOf[String]
  }

  private def diagnosticSvg(message: String): String = {
    val maxCharsPerLine = 118
    val lines = message
      .split("\n", -1)
      .toList
      .flatMap { line =>
        if (line.isEmpty) List("")
        else line.grouped(maxCharsPerLine).toList
      }
    val width = 920
    val height = math.max(140, 64 + lines.length * 18)
    val body = lines.zipWithIndex
      .map { case (line, idx) =>
        val y = 58 + idx * 18
        s"""<text x="24" y="$y">${escapeXml(line)}</text>"""
      }
      .mkString("\n")

    s"""<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}">
<rect width="100%" height="100%" fill="#fff7f7"/>
<text x="24" y="30" fill="#9f1239" font-family="ui-monospace, SFMono-Regular, Menlo, Consolas, monospace" font-size="16" font-weight="700">Typst diagnostics</text>
<g fill="#3f1d2b" font-family="ui-monospace, SFMono-Regular, Menlo, Consolas, monospace" font-size="13">
$body
</g>
</svg>"""
  }

  private def escapeXml(value: String): String = {
    value.flatMap {
      case '&'  => "&amp;"
      case '<'  => "&lt;"
      case '>'  => "&gt;"
      case '"'  => "&quot;"
      case '\'' => "&apos;"
      case c    => c.toString
    }
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
