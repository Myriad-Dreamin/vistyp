package vistyp

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation._
import org.scalajs.dom

val scopeRenamingRules = js.Dynamic.literal(
  scopes = js.Dynamic.literal(
    // vscode rules
    "namespace" -> js.Array("entity.name.namespace"),
    "type" -> js.Array("entity.name.type"),
    "type.defaultLibrary" -> js.Array("support.type"),
    "struct" -> js.Array("storage.type.struct"),
    "class" -> js.Array("entity.name.type.class"),
    "class.defaultLibrary" -> js.Array("support.class"),
    "interface" -> js.Array("entity.name.type.interface"),
    "enum" -> js.Array("entity.name.type.enum"),
    "function" -> js.Array("entity.name.function"),
    "function.defaultLibrary" -> js.Array("support.function"),
    "method" -> js.Array("entity.name.function.member"),
    "macro" -> js.Array("entity.name.function.macro"),
    "variable" -> js.Array("variable.other.readwrite , entity.name.variable"),
    "variable.readonly" -> js.Array("variable.other.constant"),
    "variable.readonly.defaultLibrary" -> js.Array("support.constant"),
    "parameter" -> js.Array("variable.parameter"),
    "property" -> js.Array("variable.other.property"),
    "property.readonly" -> js.Array("variable.other.constant.property"),
    "enumMember" -> js.Array("variable.other.enummember"),
    "event" -> js.Array("variable.other.event"),

    // typst rules
    "*.strong.emph" -> js.Array("markup.bold.typst markup.italic.typst"),
    "*.strong" -> js.Array("markup.bold.typst"),
    "*.emph" -> js.Array("markup.italic.typst"),
    "*.math" -> js.Array("markup.math.typst"),
    "bool" -> js.Array("constant.language.boolean.typst"),
    "punct" -> js.Array("punctuation.typst", "punctuation.definition.typst"),
    "escape" -> js.Array(
      "constant.character.escape.typst",
      "keyword.operator.typst",
      "punctuation.definition.typst",
    ),
    "link" -> js.Array("markup.underline.link.typst"),
    "raw" -> js.Array("markup.inline.raw.typst", "markup.raw.inline.typst"),
    "delim.math" -> js.Array(
      "punctuation.definition.math.typst",
      "punctuation.definition.string.end.math.typst",
      "string.quoted.other.typst",
    ),
    "pol" -> js.Array("variable.other.readwrite , entity.name.variable"),
  ),
)

val typstTokens = js.Array(
  "comment",
  "string",
  "keyword",
  "operator",
  "number",
  "function",
  "decorator",
  "bool",
  "punctuation",
  "escape",
  "link",
  "raw",
  "label",
  "ref",
  "heading",
  "marker",
  "term",
  "delim",
  "pol",
  "error",
  "text",
)

// todo: rewrite me in scala
def adaptVsCodeThemeForTypst(theme: js.Dynamic): js.Dynamic = {
  val tokenColors = theme.tokenColors.asInstanceOf[js.Array[js.Dynamic]]
  val semanticTokenColors = theme.semanticTokenColors.asInstanceOf[
    js.Dictionary[js.Dynamic],
  ]
  dom.console.log(tokenColors)
  dom.console.log(semanticTokenColors)

  val newTokenColors = js.Array[js.Object]()
  val defaultSettings = js.Dynamic.literal(foreground = theme.colors.foreground)
  dom.console.log("defaultSettings", defaultSettings)

  val rules = js.Map[String, js.Dynamic]()
  for (tokenColor <- tokenColors) {
    for (s <- tokenColor.scope.asInstanceOf[js.Array[String]]) {
      rules.addOne(s, tokenColor)
    }
  }

  val semanticTokenRules = js.Map[String, js.Dynamic]()
  for ((k, settings) <- semanticTokenColors) {
    semanticTokenRules.addOne(k, settings)

    if (k.startsWith("*.")) {
      val suffix = k.slice(2, k.length())
      for (token <- typstTokens) {
        semanticTokenRules.addOne(s"${token}.${suffix}", settings)
      }
    }
  }

  for (
    (k, renamedScopes) <- scopeRenamingRules.scopes
      .asInstanceOf[js.Dictionary[js.Array[String]]]
  ) {
    var scopes = js.Array[String]()
    if (k.startsWith("*.")) {
      val suffix = k.slice(2, k.length())
      for (token <- typstTokens) {
        scopes.push(s"${token}.${suffix}")
      }
    } else {
      scopes.push(k)
    }

    var settings: js.Dynamic = defaultSettings
    if (semanticTokenRules.contains(k)) {
      settings = semanticTokenRules.get(k).orUndefined.asInstanceOf[js.Dynamic]
    } else {
      for (i <- 0 until renamedScopes.length) {
        val renamedScope = renamedScopes(i)

        for (j <- renamedScope.length to 0 by -1) {
          if (j != renamedScope.length && renamedScope.charAt(j) != '.') {
            ()
          } else {
            val rule = rules.get(renamedScope.slice(0, j))
            rule.foreach { rule =>
              settings = rule.settings
            }
          }
        }
      }
    }
    newTokenColors.push(
      js.Dynamic.literal(
        name = s"typst ${k}",
        scope = scopes,
        settings = settings,
      ),
    )
  }

  theme.asInstanceOf[js.Dynamic].tokenColors =
    tokenColors.concat(newTokenColors)
  theme
}

def monacoThemeFromVscTheme(themeRaw: js.Dynamic): js.Object = {
  val vscodeTheme = adaptVsCodeThemeForTypst(themeRaw)
  val baseTokenColors = js.Array[js.Object]()
  for (tc <- vscodeTheme.tokenColors.asInstanceOf[js.Array[js.Dynamic]]) {
    def makeElement(s: String): js.Object = {
      val res = js.Dynamic.literal(token = s)
      for ((k, v) <- tc.settings.asInstanceOf[js.Dictionary[js.Any]]) {
        res.updateDynamic(k)(v)
      }
      res
    }

    if (js.typeOf(tc.scope) == "string") {
      baseTokenColors.push(makeElement(tc.scope.asInstanceOf[String]))
    } else {
      val elements = tc.scope.asInstanceOf[js.Array[String]].map(makeElement);
      for (element <- elements) {
        baseTokenColors.push(element)
      }
    }
  }

  val res = js.Dynamic.literal(
    base = "vs-dark",
    inherit = true,
    colors = vscodeTheme.colors,
    rules = baseTokenColors,
  )

  res
}

@js.native @JSImport("/assets/tokyo-night.json", JSImport.Namespace)
val tokyoNightThemeData: js.Dynamic = js.native

def tokyoNightTheme(): js.Object = {
  monacoThemeFromVscTheme(
    // todo: readonly tokyoNightThemeData
    js.JSON.parse(js.JSON.stringify(tokyoNightThemeData)),
  )
}

class SemanticTokensProvider(legend: js.Object) {
  @JSExport
  def getLegend(): js.Object = legend

  @JSExport
  def provideDocumentSemanticTokens(
      model: js.Dynamic,
      lastResultId: String,
  ): js.Promise[js.Object] = {
    val content = model.getValue().asInstanceOf[String]
    // todo: support incremental update
    TypstTs
      .addSource("/semantic-tokens.typ", content)
      .`then`(_ =>
        TypstTs.getSemanticTokens(
          js.Dynamic.literal(mainFilePath = "/semantic-tokens.typ"),
        ),
      )
  }

  @JSExport
  def releaseDocumentSemanticTokens(resultId: String): Unit = {}
}
