/**
 * *****************************************************************************
 * Copyright 2013 Roland Ewald
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * ****************************************************************************
 */
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
    "§" -> "S").map(
      x => (x._1, x._2 + " ")) //This avoids: <T> => \textlessT(<-undefined control sequence)

  /** Character replacement map. */
  val charReplacements = (escapeSymbols.map(x => (x, x)).toMap ++ replaceSymbols).map(x => (x._1, """\\\""" + x._2))

  /** The regular expression for CamelCase words.*/
  val camelCase = """([a-z])([A-Z])""".r

  /** To be used for '\n' in in-line code. */
  val newlineCharCodeInline = "^^J"

  /** To be prefixed with a '\' in in-line code. */
  val inlineCodeReplaceChars = Set("""\\""", "{", "}", "%")

  /**
   * Converts string to LaTeX-friendly string (special characters are escaped etc.).
   *  @param s string the string to be converted
   */
  def convertSpecialChars(s: String) = convertChars(charReplacements)(s)

  /**
   * Adds Latex markers for hyphenation ('\-').
   *  @param s method or class name
   */
  def camelCaseHyphenation(s: String): String = {
    camelCase.replaceAllIn(s, {
      m => m.group(1) + """\\-""" + m.group(2)
    })
  }

  /**
   * Converts code so that it can be in-lined.
   * @see sec. 5.1. of the [[ftp://ftp.tex.ac.uk/tex-archive/macros/latex/contrib/listings/listings.pdf lstlistings manual]] ("Listings inside arguments")
   * @param s code to be converted
   */
  def codeToInline(s: String): String = {
    val charsConverted = convertChars(inlineCodeReplaceChars.map(x => (x, """\\\""" + x)).toMap + ("""\""" -> """\\\\"""))(s)
    val doubleSpacesEscaped = charsConverted.replaceAll("""\s\s""", """\\ \\ """)
    val firstLineSpacesEscaped = doubleSpacesEscaped.replaceAll("""^\s""", """\\ """)
    newlineCharCodeInline + firstLineSpacesEscaped.replaceAll("\n", newlineCharCodeInline + "\n")
  }

  /**
   * Maps a set of characters to other characters in a given string.
   * @param replacements the conversion map
   * @param s the string to be converted
   */
  def convertChars(replacements: Map[String, String])(s: String): String = {
    val regex = ("[" + (replacements.keySet - """\""").mkString + "]").r //'\' must not be included, as it will escape the next special character
    regex.replaceAllIn(s, {
      m => replacements.getOrElse(m.group(0), m.group(0))
    })
  }

  /**
   * Produce label that only contains allowed characters (all others are replaced by ':').
   * @see [[http://tex.stackexchange.com/a/18312/2569 Latex chars that can be used in labels]]
   * @param s label name
   */
  def produceValidLabel(s: String): String = {
    val charsToReplace = inlineCodeReplaceChars + "~" + "#"
    convertChars(charsToReplace.map(x => (x, ":")).toMap + ("""\""" -> ":"))(s)
  }

}