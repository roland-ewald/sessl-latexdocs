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
      x => (x._1, x._2 + " ")) //This avoids: <T> => \textlessT(<-undefined control sequene)

  /** Character replacement map. */
  val charReplacements = (escapeSymbols.map(x => (x, x)).toMap ++ replaceSymbols).map(x => (x._1, """\\\""" + x._2))

  /** The regular expression for character conversion. */
  val specialCharConversion = ("[" + (escapeSymbols ++ replaceSymbols.keys) + "]").r

  /** The regular expression for CamelCase words.*/
  val camelCase = """([a-z])([A-Z])""".r

  /**
   * Converts string to LaTeX-friendly string (special characters are escaped etc.).
   *  @param s string the string to be converted
   */
  def convertSpecialChars(s: String): String = specialCharConversion.replaceAllIn(s, {
    m => charReplacements.getOrElse(m.group(0), m.group(0))
  })

  /**
   * Adds Latex markers for hyphenation ('\-').
   *  @param s method or class name
   */
  def camelCaseHyphenation(s: String): String = camelCase.replaceAllIn(s, {
    m => m.group(1) + """\\-""" + m.group(2)
  })

}