package vistyp

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

import org.scalajs.dom

case class AssetArg(name: String, defaultValue: String, kind: String)

case class AssetPackageKey(namespace: String, name: String, version: String):
  def id: String = s"@$namespace/$name:$version"

case class AssetResource(
    id: String,
    functionName: String,
    packageLabel: String,
    packageKey: Option[AssetPackageKey],
    args: List[AssetArg],
)

case class LoadedAssetPackage(
    key: AssetPackageKey,
    entrypoint: String,
    entrypointVirtualPath: String,
    files: Map[String, String],
    resources: List[AssetResource],
)

case class AssetLibraryState(
    sourceUrl: String = "",
    loading: Boolean = false,
    packages: List[LoadedAssetPackage] = Nil,
    error: Option[String] = None,
)

case class ResourceRef(
    namespace: String,
    packageName: String,
    version: String,
    functionName: String,
):
  def packageKey: AssetPackageKey =
    AssetPackageKey(namespace, packageName, version)
  def id: String = s"@$namespace/$packageName:$version/$functionName"

object ResourceRef:
  private val Pattern = raw"^@([^/]+)/([^:]+):([^/]+)/(.+)$$".r

  def parse(raw: String): Option[ResourceRef] =
    raw match
      case Pattern(namespace, packageName, version, functionName) =>
        Some(ResourceRef(namespace, packageName, version, functionName))
      case _ => None

object BuiltinAssets:
  val resources: List[AssetResource] = List(
    AssetResource(
      id = "x-circle",
      functionName = "x-circle",
      packageLabel = "Builtin",
      packageKey = None,
      args = List(
        AssetArg("rad", "50", "number"),
        AssetArg("inner-text", "\"\"", "text"),
      ),
    ),
    AssetResource(
      id = "x-rect",
      functionName = "x-rect",
      packageLabel = "Builtin",
      packageKey = None,
      args = List(
        AssetArg("x", "100", "number"),
        AssetArg("y", "none", "code"),
        AssetArg("inner-text", "\"\"", "text"),
      ),
    ),
    AssetResource(
      id = "x-arrow",
      functionName = "x-arrow",
      packageLabel = "Builtin",
      packageKey = None,
      args = List(
        AssetArg("start", "(0, 10)", "code"),
        AssetArg("end", "(50, 10)", "code"),
        AssetArg("inner-text", "\"\"", "text"),
        AssetArg("mark", "(end: \">\")", "code"),
      ),
    ),
  )

sealed trait AssetPackageLoad
object AssetPackageLoad:
  case object Universe extends AssetPackageLoad
  case class HttpArchive(url: String) extends AssetPackageLoad
  case class GitPath(path: String) extends AssetPackageLoad

case class AssetPackageIndexEntry(
    namespace: String,
    name: String,
    version: String,
    entrypoint: String,
    load: AssetPackageLoad,
    integrity: Option[String],
)

case class PackageToml(name: String, version: String, entrypoint: String)

object AssetLibraryLoader:
  private val thetaLoad = "θload"
  private val thetaIntegrity = "θintegrity"

  def loadIndex(indexUrl: String): js.Promise[List[LoadedAssetPackage]] =
    val gitSource = GitSource.parse(indexUrl)
    fetchText(indexUrl)
      .`then`((indexText: String) => {
        val entries = parseIndex(indexText)
        val loaders = entries.map(entry => () => loadPackage(entry, gitSource))
        sequence(loaders, List.empty[LoadedAssetPackage])
      })
      .asInstanceOf[js.Promise[List[LoadedAssetPackage]]]

  private def loadPackage(
      entry: AssetPackageIndexEntry,
      gitSource: Option[GitSource],
  ): js.Promise[Option[LoadedAssetPackage]] =
    entry.load match
      case AssetPackageLoad.Universe =>
        js.Promise.resolve(None)
      case AssetPackageLoad.HttpArchive(url) =>
        entry.integrity match
          case Some(integrity) =>
            fetchPackageArchive(resolveHttpArchiveUrl(url, entry), integrity)
              .`then`((files: Map[String, String]) =>
                Some(materializePackage(entry, files)),
              )
              .asInstanceOf[js.Promise[Option[LoadedAssetPackage]]]
          case None =>
            failed(
              s"${entry.name}@${entry.version}: HTTP θload requires θintegrity",
            )
      case AssetPackageLoad.GitPath(path) =>
        gitSource match
          case Some(source) =>
            val packagePath = source.resolve(path)
            fetchGitDirectory(source, packagePath)
              .`then`((files: Map[String, String]) =>
                Some(materializePackage(entry, files)),
              )
              .asInstanceOf[js.Promise[Option[LoadedAssetPackage]]]
          case None =>
            failed(
              s"${entry.name}@${entry.version}: relative θload requires a recognized Git index source",
            )

  private def fetchPackageArchive(
      url: String,
      integrity: String,
  ): js.Promise[Map[String, String]] =
    val parsedIntegrity = parseIntegrity(integrity)
    new js.Promise[Map[String, String]]((resolve, reject) => {
      fetchArrayBuffer(url)
        .`then`((buffer: ArrayBuffer) => {
          sha256Hex(buffer)
            .`then`((actualDigest: String) => {
              if actualDigest != parsedIntegrity then
                reject(
                  new Exception(
                    s"Integrity mismatch for $url: expected $parsedIntegrity, got $actualDigest",
                  ),
                )
              else
                decompressGzip(buffer)
                  .`then`((decompressed: ArrayBuffer) => {
                    try resolve(parseTar(decompressed))
                    catch case error: Throwable => reject(error)
                  })
                  .`catch`((error: Any) => reject(error))
            })
            .`catch`((error: Any) => reject(error))
        })
        .`catch`((error: Any) => reject(error))
    })

  private def parseIndex(indexText: String): List[AssetPackageIndexEntry] =
    val raw = js.JSON.parse(indexText)
    if !js.Array.isArray(raw) then
      throw new Exception("Package index must be a JSON array")

    raw
      .asInstanceOf[js.Array[js.Dynamic]]
      .toList
      .zipWithIndex
      .map { case (entry, index) =>
        val loadRaw = dynString(entry, thetaLoad)
        val load = parseLoad(loadRaw)
        val namespace = load match
          case AssetPackageLoad.Universe => "preview"
          case _                         => "vistyp"

        AssetPackageIndexEntry(
          namespace = namespace,
          name = requiredString(entry, "name", index),
          version = requiredString(entry, "version", index),
          entrypoint = requiredString(entry, "entrypoint", index),
          load = load,
          integrity = dynString(entry, thetaIntegrity),
        )
      }

  private def parseLoad(raw: Option[String]): AssetPackageLoad =
    raw.filter(_.nonEmpty) match
      case None | Some("universe") => AssetPackageLoad.Universe
      case Some(url) if url.startsWith("https://") || url.startsWith("http://") =>
        AssetPackageLoad.HttpArchive(url)
      case Some(path)
          if path.startsWith("./") || path.startsWith("../") || path.startsWith(
            "/",
          ) =>
        AssetPackageLoad.GitPath(path)
      case Some(value) => throw new Exception(s"Unsupported θload value: $value")

  private def resolveHttpArchiveUrl(
      rawUrl: String,
      entry: AssetPackageIndexEntry,
  ): String =
    val templated = rawUrl
      .replace("{name}", entry.name)
      .replace("{version}", entry.version)
    if templated.endsWith("/") then
      s"$templated${entry.name}-${entry.version}.tar.gz"
    else templated

  private def requiredString(
      entry: js.Dynamic,
      key: String,
      index: Int,
  ): String =
    dynString(entry, key).getOrElse {
      throw new Exception(s"Package index entry #$index is missing '$key'")
    }

  private def dynString(entry: js.Dynamic, key: String): Option[String] =
    val value = entry.selectDynamic(key)
    if js.isUndefined(value) || value == null then None
    else Some(value.asInstanceOf[String])

  private def parseIntegrity(value: String): String =
    val prefix = "sha256:"
    if !value.startsWith(prefix) then
      throw new Exception(s"Unsupported θintegrity algorithm: $value")

    val digest = value.stripPrefix(prefix)
    if !digest.matches("[0-9a-f]{64}") then
      throw new Exception(s"Invalid sha256 θintegrity digest: $value")
    digest

  private def materializePackage(
      entry: AssetPackageIndexEntry,
      files: Map[String, String],
  ): LoadedAssetPackage =
    val normalizedFiles = files.map { case (path, content) =>
      normalizePath(path) -> content
    }
    val packageToml = normalizedFiles
      .get("typst.toml")
      .getOrElse(
        throw new Exception(s"${entry.name}@${entry.version}: missing typst.toml"),
      )
    val parsedToml = PackageTomlParser.parse(packageToml)
    if parsedToml.name != entry.name then
      throw new Exception(
        s"${entry.name}@${entry.version}: typst.toml name is ${parsedToml.name}",
      )
    if parsedToml.version != entry.version then
      throw new Exception(
        s"${entry.name}@${entry.version}: typst.toml version is ${parsedToml.version}",
      )
    if parsedToml.entrypoint != entry.entrypoint then
      throw new Exception(
        s"${entry.name}@${entry.version}: typst.toml entrypoint is ${parsedToml.entrypoint}",
      )

    val entrypointSource = normalizedFiles
      .get(entry.entrypoint)
      .getOrElse(
        throw new Exception(
          s"${entry.name}@${entry.version}: missing entrypoint ${entry.entrypoint}",
        ),
      )
    val key = AssetPackageKey(entry.namespace, entry.name, entry.version)
    val virtualRoot = s"/@packages/${key.namespace}/${key.name}/${key.version}"
    val virtualFiles = normalizedFiles.map { case (path, content) =>
      s"$virtualRoot/$path" -> content
    }
    LoadedAssetPackage(
      key = key,
      entrypoint = entry.entrypoint,
      entrypointVirtualPath = s"$virtualRoot/${entry.entrypoint}",
      files = virtualFiles,
      resources = AssetResourceScanner.scan(
        key,
        entrypointSource,
        s"${entry.name}@${entry.version}",
      ),
    )

  private def sequence(
      loaders: List[() => js.Promise[Option[LoadedAssetPackage]]],
      acc: List[LoadedAssetPackage],
  ): js.Promise[List[LoadedAssetPackage]] =
    loaders match
      case Nil => js.Promise.resolve(acc.reverse)
      case next :: rest =>
        next().`then`((loaded: Option[LoadedAssetPackage]) =>
          sequence(rest, loaded.fold(acc)(_ :: acc)),
        )

  private def fetchText(url: String): js.Promise[String] =
    fetch(url)
      .`then`((response: js.Dynamic) =>
        response.text().asInstanceOf[js.Promise[String]],
      )
      .asInstanceOf[js.Promise[String]]

  private def fetchArrayBuffer(url: String): js.Promise[ArrayBuffer] =
    fetch(url)
      .`then`((response: js.Dynamic) =>
        response.arrayBuffer().asInstanceOf[js.Promise[ArrayBuffer]],
      )
      .asInstanceOf[js.Promise[ArrayBuffer]]

  private def fetch(url: String): js.Promise[js.Dynamic] =
    dom.window
      .asInstanceOf[js.Dynamic]
      .fetch(url)
      .asInstanceOf[js.Promise[js.Dynamic]]
      .`then`((response: js.Dynamic) => {
        val ok = response.ok.asInstanceOf[Boolean]
        if !ok then
          val status = response.status.asInstanceOf[Int]
          throw new Exception(s"Fetch failed for $url: HTTP $status")
        response
      })

  private def sha256Hex(buffer: ArrayBuffer): js.Promise[String] =
    dom.window
      .asInstanceOf[js.Dynamic]
      .crypto
      .subtle
      .digest("SHA-256", buffer)
      .asInstanceOf[js.Promise[ArrayBuffer]]
      .`then`((digest: ArrayBuffer) => {
        val bytes = new Uint8Array(digest)
        (0 until bytes.length)
          .map(i => f"${bytes(i).toInt & 0xff}%02x")
          .mkString
      })

  private def decompressGzip(buffer: ArrayBuffer): js.Promise[ArrayBuffer] =
    new js.Promise[ArrayBuffer]((resolve, reject) => {
      val ctor = js.Dynamic.global.selectDynamic("DecompressionStream")
      if js.isUndefined(ctor) || ctor == null then
        reject(
          new Exception(
            "This browser does not support DecompressionStream for package.tar.gz",
          ),
        )
      else
        try
          val stream = js.Dynamic.newInstance(ctor)("gzip")
          val blob = js.Dynamic.newInstance(js.Dynamic.global.Blob)(js.Array(buffer))
          val readable = blob.stream().pipeThrough(stream)
          val response = js.Dynamic.newInstance(js.Dynamic.global.Response)(readable)
          response
            .arrayBuffer()
            .asInstanceOf[js.Promise[ArrayBuffer]]
            .`then`((result: ArrayBuffer) => resolve(result))
            .`catch`((error: Any) => reject(error))
        catch
          case error: Throwable => reject(error)
    })

  private def parseTar(buffer: ArrayBuffer): Map[String, String] =
    val bytes = new Uint8Array(buffer)
    val decoder = js.Dynamic.newInstance(js.Dynamic.global.TextDecoder)("utf-8")
    var offset = 0
    var files = Map.empty[String, String]

    while offset + 512 <= bytes.length do
      if isEmptyBlock(bytes, offset) then return files

      val name = readHeaderString(bytes, offset, 0, 100)
      val prefix = readHeaderString(bytes, offset, 345, 155)
      val path =
        if prefix.nonEmpty then s"$prefix/$name" else name
      val size = readOctal(bytes, offset, 124, 12)
      val typeFlag = bytes(offset + 156).toInt & 0xff
      val dataOffset = offset + 512

      if path.nonEmpty && (typeFlag == 0 || typeFlag == '0'.toInt) then
        val chunk = bytes
          .asInstanceOf[js.Dynamic]
          .slice(dataOffset, dataOffset + size.toInt)
          .asInstanceOf[Uint8Array]
        files += normalizePath(path) -> decoder.decode(chunk).asInstanceOf[String]

      val paddedSize = ((size + 511L) / 512L) * 512L
      offset = dataOffset + paddedSize.toInt

    files

  private def isEmptyBlock(bytes: Uint8Array, offset: Int): Boolean =
    var idx = 0
    while idx < 512 do
      if (bytes(offset + idx).toInt & 0xff) != 0 then return false
      idx += 1
    true

  private def readHeaderString(
      bytes: Uint8Array,
      headerOffset: Int,
      fieldOffset: Int,
      length: Int,
  ): String =
    val builder = new StringBuilder
    var idx = 0
    while idx < length do
      val value = bytes(headerOffset + fieldOffset + idx).toInt & 0xff
      if value == 0 then return builder.toString.trim
      builder.append(value.toChar)
      idx += 1
    builder.toString.trim

  private def readOctal(
      bytes: Uint8Array,
      headerOffset: Int,
      fieldOffset: Int,
      length: Int,
  ): Long =
    val raw = readHeaderString(bytes, headerOffset, fieldOffset, length).trim
    if raw.isEmpty then 0L else java.lang.Long.parseLong(raw, 8)

  private def fetchGitDirectory(
      source: GitSource,
      packagePath: String,
  ): js.Promise[Map[String, String]] =
    fetchGitPath(source, normalizePath(packagePath), normalizePath(packagePath))

  private def fetchGitPath(
      source: GitSource,
      path: String,
      rootPath: String,
  ): js.Promise[Map[String, String]] =
    fetchJson(source.contentsApiUrl(path))
      .`then`((value: js.Any) => {
        if js.Array.isArray(value) then
          val items = value.asInstanceOf[js.Array[js.Dynamic]].toList
          val loaders = items.map(item => () => fetchGitItem(source, item, rootPath))
          sequenceMaps(loaders, Map.empty[String, String])
        else fetchGitItem(source, value.asInstanceOf[js.Dynamic], rootPath)
      })
      .asInstanceOf[js.Promise[Map[String, String]]]

  private def fetchGitItem(
      source: GitSource,
      item: js.Dynamic,
      rootPath: String,
  ): js.Promise[Map[String, String]] =
    dynString(item, "type") match
      case Some("dir") =>
        fetchGitPath(source, requiredGitString(item, "path"), rootPath)
      case Some("file") =>
        val itemPath = requiredGitString(item, "path")
        val relativePath = relativeTo(rootPath, itemPath)
        fetchText(requiredGitString(item, "download_url"))
          .`then`((content: String) => Map(relativePath -> content))
      case _ => js.Promise.resolve(Map.empty[String, String])

  private def sequenceMaps(
      loaders: List[() => js.Promise[Map[String, String]]],
      acc: Map[String, String],
  ): js.Promise[Map[String, String]] =
    loaders match
      case Nil => js.Promise.resolve(acc)
      case next :: rest =>
        next().`then`((files: Map[String, String]) =>
          sequenceMaps(rest, acc ++ files),
        )

  private def fetchJson(url: String): js.Promise[js.Any] =
    fetch(url)
      .`then`((response: js.Dynamic) =>
        response.json().asInstanceOf[js.Promise[js.Any]],
      )
      .asInstanceOf[js.Promise[js.Any]]

  private def requiredGitString(item: js.Dynamic, key: String): String =
    dynString(item, key).getOrElse {
      throw new Exception(s"GitHub API item is missing '$key'")
    }

  private def relativeTo(rootPath: String, itemPath: String): String =
    val root = normalizePath(rootPath)
    val item = normalizePath(itemPath)
    if root.isEmpty then item
    else item.stripPrefix(root).stripPrefix("/")

  private def failed[A](message: String): js.Promise[A] =
    new js.Promise[A]((_, reject) => reject(new Exception(message)))

  def normalizePath(path: String): String =
    val parts = path
      .replace('\\', '/')
      .split("/")
      .toList
      .filter(_.nonEmpty)
    val normalized = parts.foldLeft(List.empty[String]) {
      case (acc, ".")  => acc
      case (Nil, "..") => Nil
      case (acc, "..") => acc.dropRight(1)
      case (acc, part) => acc :+ part
    }
    normalized.mkString("/")

object PackageTomlParser:
  def parse(source: String): PackageToml =
    val packageValues = parsePackageSection(source)
    PackageToml(
      name = requireValue(packageValues, "name"),
      version = requireValue(packageValues, "version"),
      entrypoint = requireValue(packageValues, "entrypoint"),
    )

  private def parsePackageSection(source: String): Map[String, String] =
    var inPackage = false
    var values = Map.empty[String, String]

    source.linesIterator.foreach { rawLine =>
      val line = stripComment(rawLine).trim
      if line.nonEmpty then
        if line.startsWith("[") && line.endsWith("]") then
          inPackage = line == "[package]"
        else if inPackage then
          val eq = line.indexOf("=")
          if eq > 0 then
            val key = line.take(eq).trim
            val value = parseTomlString(line.drop(eq + 1).trim)
            value.foreach(parsed => values += key -> parsed)
    }

    values

  private def requireValue(values: Map[String, String], key: String): String =
    values.getOrElse(key, throw new Exception(s"typst.toml is missing $key"))

  private def parseTomlString(value: String): Option[String] =
    if value.startsWith("\"") && value.endsWith("\"") then
      Some(unescapeTomlString(value.drop(1).dropRight(1)))
    else None

  private def unescapeTomlString(value: String): String =
    val builder = new StringBuilder
    var escaped = false
    value.foreach { ch =>
      if escaped then
        builder.append(ch match
          case 'n'  => '\n'
          case 't'  => '\t'
          case 'r'  => '\r'
          case '"'  => '"'
          case '\\' => '\\'
          case other => other,
        )
        escaped = false
      else if ch == '\\' then escaped = true
      else builder.append(ch)
    }
    builder.toString

  private def stripComment(line: String): String =
    val builder = new StringBuilder
    var inString = false
    var escaped = false
    var idx = 0
    while idx < line.length do
      val ch = line.charAt(idx)
      if escaped then
        builder.append(ch)
        escaped = false
      else if ch == '\\' && inString then
        builder.append(ch)
        escaped = true
      else if ch == '"' then
        builder.append(ch)
        inString = !inString
      else if ch == '#' && !inString then return builder.toString
      else builder.append(ch)
      idx += 1
    builder.toString

object AssetResourceScanner:
  def scan(
      key: AssetPackageKey,
      source: String,
      packageLabel: String,
  ): List[AssetResource] =
    var offset = 0
    var resources = List.empty[AssetResource]
    while offset < source.length do
      val letOffset = source.indexOf("#let", offset)
      if letOffset < 0 then offset = source.length
      else
        scanLet(key, source, letOffset, packageLabel).foreach { resource =>
          resources = resource :: resources
        }
        offset = letOffset + 4

    resources.reverse

  private def scanLet(
      key: AssetPackageKey,
      source: String,
      letOffset: Int,
      packageLabel: String,
  ): Option[AssetResource] =
    if !hasBoundaryBefore(source, letOffset) then return None
    var idx = letOffset + "#let".length
    idx = skipWhitespace(source, idx)
    val (name, nameEnd) = readIdentifier(source, idx)
    if !name.startsWith("x-") then return None
    idx = skipWhitespace(source, nameEnd)
    if idx >= source.length || source.charAt(idx) != '(' then return None
    val close = findMatching(source, idx, '(', ')')
    if close < 0 then return None
    val params = source.substring(idx + 1, close)
    parseParams(params).map(args =>
      AssetResource(
        id = ResourceRef(key.namespace, key.name, key.version, name).id,
        functionName = name,
        packageLabel = packageLabel,
        packageKey = Some(key),
        args = args,
      ),
    )

  private def parseParams(params: String): Option[List[AssetArg]] =
    val parts = splitTopLevel(params, ',').map(_.trim).filter(_.nonEmpty)
    var sawNodeLabel = false
    var args = List.empty[AssetArg]
    var valid = true

    parts.foreach { part =>
      if valid then
        if part.startsWith("..") then valid = false
        else
          val colon = findTopLevel(part, ':')
          if colon < 0 then valid = false
          else
            val name = part.take(colon).trim
            val defaultValue = part.drop(colon + 1).trim
            if !isIdentifier(name) || defaultValue.isEmpty then valid = false
            else if name == "node-label" then sawNodeLabel = true
            else args = args :+ AssetArg(name, defaultValue, guessKind(defaultValue))
    }

    if valid && sawNodeLabel then Some(args) else None

  private def splitTopLevel(source: String, delimiter: Char): List[String] =
    var parts = List.empty[String]
    var start = 0
    var idx = 0
    var paren = 0
    var bracket = 0
    var brace = 0
    var inString = false
    var escaped = false
    while idx < source.length do
      val ch = source.charAt(idx)
      if escaped then escaped = false
      else if inString then
        if ch == '\\' then escaped = true
        else if ch == '"' then inString = false
      else
        ch match
          case '"' => inString = true
          case '(' => paren += 1
          case ')' => paren -= 1
          case '[' => bracket += 1
          case ']' => bracket -= 1
          case '{' => brace += 1
          case '}' => brace -= 1
          case `delimiter` if paren == 0 && bracket == 0 && brace == 0 =>
            parts = parts :+ source.substring(start, idx)
            start = idx + 1
          case _ =>
      idx += 1
    parts :+ source.substring(start)

  private def findTopLevel(source: String, needle: Char): Int =
    splitTopLevel(source, needle).headOption match
      case Some(head) if head.length < source.length => head.length
      case _                                        => -1

  private def findMatching(
      source: String,
      openOffset: Int,
      open: Char,
      close: Char,
  ): Int =
    var depth = 0
    var idx = openOffset
    var inString = false
    var escaped = false
    while idx < source.length do
      val ch = source.charAt(idx)
      if escaped then escaped = false
      else if inString then
        if ch == '\\' then escaped = true
        else if ch == '"' then inString = false
      else if ch == '"' then inString = true
      else if ch == open then depth += 1
      else if ch == close then
        depth -= 1
        if depth == 0 then return idx
      idx += 1
    -1

  private def readIdentifier(source: String, offset: Int): (String, Int) =
    var idx = offset
    val builder = new StringBuilder
    while idx < source.length && isIdentifierChar(source.charAt(idx)) do
      builder.append(source.charAt(idx))
      idx += 1
    builder.toString -> idx

  private def skipWhitespace(source: String, offset: Int): Int =
    var idx = offset
    while idx < source.length && source.charAt(idx).isWhitespace do idx += 1
    idx

  private def hasBoundaryBefore(source: String, offset: Int): Boolean =
    offset == 0 || !isIdentifierChar(source.charAt(offset - 1))

  private def isIdentifier(value: String): Boolean =
    value.nonEmpty &&
      (value.head.isLetter || value.head == '_') &&
      value.forall(isIdentifierChar)

  private def isIdentifierChar(ch: Char): Boolean =
    ch.isLetterOrDigit || ch == '_' || ch == '-'

  private def guessKind(defaultValue: String): String =
    val value = defaultValue.trim
    if value == "true" || value == "false" then "boolean"
    else if value.startsWith("\"") && value.endsWith("\"") then "text"
    else if value.matches("[+-]?[0-9]+(\\.[0-9]+)?") then "number"
    else if value.startsWith("rgb(") || value.startsWith("luma(") then "color"
    else "code"

case class GitSource(
    owner: String,
    repo: String,
    revision: String,
    indexPath: String,
):
  def resolve(path: String): String =
    if path.startsWith("/") then
      AssetLibraryLoader.normalizePath(path.stripPrefix("/"))
    else
      val base = indexPath.split("/").toList.dropRight(1).mkString("/")
      AssetLibraryLoader.normalizePath(s"$base/$path")

  def contentsApiUrl(path: String): String =
    val encodedPath = path
      .split("/")
      .filter(_.nonEmpty)
      .map(js.URIUtils.encodeURIComponent)
      .mkString("/")
    val encodedRef = js.URIUtils.encodeURIComponent(revision)
    s"https://api.github.com/repos/$owner/$repo/contents/$encodedPath?ref=$encodedRef"

object GitSource:
  def parse(url: String): Option[GitSource] =
    try
      val parsed = new dom.URL(url)
      parsed.hostname match
        case "raw.githubusercontent.com" => parseRawGitHub(parsed)
        case "github.com"               => parseGitHubBlob(parsed)
        case _                          => None
    catch case _: Throwable => None

  private def parseRawGitHub(url: dom.URL): Option[GitSource] =
    val parts = url.pathname.stripPrefix("/").split("/").toList
    parts match
      case owner :: repo :: revision :: rest if rest.nonEmpty =>
        Some(GitSource(owner, repo, revision, rest.mkString("/")))
      case _ => None

  private def parseGitHubBlob(url: dom.URL): Option[GitSource] =
    val parts = url.pathname.stripPrefix("/").split("/").toList
    parts match
      case owner :: repo :: "blob" :: revision :: rest if rest.nonEmpty =>
        Some(GitSource(owner, repo, revision, rest.mkString("/")))
      case _ => None
