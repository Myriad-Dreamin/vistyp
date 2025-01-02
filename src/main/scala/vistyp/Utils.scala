package vistyp

private val escapeStrPattern = "((?:\\r\\n)|[\"\\\\\\t\\f\\r\\n])".r
def escapeStr(s: String): String = {
  escapeStrPattern.replaceAllIn(
    s,
    (m) => {
      m.group(1) match {
        case "\""   => "\\\\\""
        case "\\"   => "\\\\\\\\"
        case "\n"   => "\\\\n"
        case "\t"   => "\\\\t"
        case "\r"   => "\\\\r"
        case "\f"   => "\\\\f"
        case "\r\n" => "\\\\n"
        case other  => "\\" + other
      }
    },
  )
}

private val unescapeStrPattern = "(\\\\[\\\\tfrn\\\"])".r
def unescapeStr(s: String): String = {
  unescapeStrPattern.replaceAllIn(
    s,
    (m) => {
      m.group(1) match {
        case "\\\"" => "\""
        case "\\\\" => "\\"
        case "\\n"  => "\n"
        case "\\t"  => "\t"
        case "\\r"  => "\r"
        case "\\f"  => "\f"
        case other  => other
      }
    },
  )
}
