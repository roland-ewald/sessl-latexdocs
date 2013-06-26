package sessl.utils.doclet

/**
 * A simple string converter for LaTeX support.
 *
 * @author Roland Ewald
 */
object LatexStringConverter {

  /** Symbols that are simply displayed as '\s' (s being the symbol). */
  val escapeSymbols = Set("_", "$", "<", ">", "%", "{", "}", "#", "&")

  /** Symbols to be replaced by Latex control sequences. */
  val replaceSymbols = Map(
    "<" -> "textless",
    ">" -> "textgreater",
    """\\""" -> "workaround, see key '\\' below", // '\' is used as escape character in regular expressions as well :)
    "\\" -> "textbackslash",
    "§" -> "S")

  /** Replacement map. */
  val replacements = (escapeSymbols.map(x => (x, x)).toMap ++ replaceSymbols).map(x => (x._1, """\\\""" + x._2))

  /** The regular expression to match. */
  val regexp = "[" + (escapeSymbols ++ replaceSymbols.keys) + "]"

  /**
   * Converts string to LaTeX-friendly string (special characters are escaped etc.).
   *  @param s string the string to be converted
   */
  def convert(s: String): String = regexp.r.replaceAllIn(s, {
    m => replacements.getOrElse(m.group(0), m.group(0))
  })

}