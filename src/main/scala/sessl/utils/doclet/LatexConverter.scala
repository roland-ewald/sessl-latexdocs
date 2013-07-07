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

import scala.tools.nsc.doc.base.LinkToExternal
import scala.tools.nsc.doc.base.LinkToMember
import scala.tools.nsc.doc.base.LinkToTpl
import scala.tools.nsc.doc.base.Tooltip
import scala.tools.nsc.doc.base.comment.Block
import scala.tools.nsc.doc.base.comment.Body
import scala.tools.nsc.doc.base.comment.Bold
import scala.tools.nsc.doc.base.comment.Chain
import scala.tools.nsc.doc.base.comment.Code
import scala.tools.nsc.doc.base.comment.Comment
import scala.tools.nsc.doc.base.comment.DefinitionList
import scala.tools.nsc.doc.base.comment.EntityLink
import scala.tools.nsc.doc.base.comment.HorizontalRule
import scala.tools.nsc.doc.base.comment.HtmlTag
import scala.tools.nsc.doc.base.comment.Inline
import scala.tools.nsc.doc.base.comment.Italic
import scala.tools.nsc.doc.base.comment.Link
import scala.tools.nsc.doc.base.comment.Monospace
import scala.tools.nsc.doc.base.comment.OrderedList
import scala.tools.nsc.doc.base.comment.Paragraph
import scala.tools.nsc.doc.base.comment.Subscript
import scala.tools.nsc.doc.base.comment.Summary
import scala.tools.nsc.doc.base.comment.Superscript
import scala.tools.nsc.doc.base.comment.Text
import scala.tools.nsc.doc.base.comment.Title
import scala.tools.nsc.doc.base.comment.Underline
import scala.tools.nsc.doc.base.comment.UnorderedList

import com.weiglewilczek.slf4s.Logging

import LatexStringConverter.convertSpecialChars
import LatexStringConverter.produceValidLabel

/**
 * Auxiliary methods for Latex code generation, used by [[TypeOrPackageWrapper]] and other wrappers.
 *
 * @see [[CommentedEntity]]
 *
 * @author Roland Ewald
 *
 */
object LatexConverter extends Logging {

  def convertSummary(co: Option[Comment]): String =
    (for (c <- co; sum <- c.body.summary) yield convertInline(sum)).getOrElse("")

  def convertComment(c: Option[Comment]): String =
    c.map(x => convertCommentBody(x.body)).getOrElse("")

  def convertCommentBody(b: Body): String =
    b.blocks.map(convertBlock).mkString

  def convertBlock(b: Block): String = b match {
    case Title(text, level) => sectionByLevel(level) + "{" + convertInline(text) + "}\n\n"
    case Paragraph(text) => "\n" + convertInline(text) + "\n"
    case Code(data) => handleCodeSample(data)
    case UnorderedList(items) => env("itemize", items.map(convertBlock).map(item).mkString)
    case OrderedList(items, style) => env("enumerate", items.map(convertBlock).map(item).mkString)
    case DefinitionList(items) => env("itemize", items.map(x =>
      "\\textbf{" + convertInline(x._1) + "}:" + convertBlock(x._2)).map(item).mkString)
    case h: HorizontalRule => "\n\\hline\n"
  }

  def convertInline(i: Inline): String = i match {
    case Chain(items) => items.map(convertInline).mkString("")
    case Summary(text) => convertInline(text)
    case Text(text) => convertSpecialChars(text)
    case Underline(text) => s"\\underline{${convertInline(text)}}"
    case Bold(text) => s"\\textbf{${convertInline(text)}}"
    case Italic(text) => s"\\emph{${convertInline(text)}}"
    case Superscript(text) => s"\\textsuperscript{${convertInline(text)}}"
    case Subscript(text) => s"\\textsubscript{${convertInline(text)}}"
    case Link(target, title) => handleExternalLink(target, convertInline(title))
    case Monospace(text) => s"\\texttt{${convertInline(text)}}"
    case el: EntityLink => handleEntityLink(el)
    case html: HtmlTag =>
      logger.warn("Ignoring HTML tag '" + html + "'"); ""
  }

  def handleEntityLink(el: EntityLink): String = el.link match {
    case LinkToMember(mem, tpl) => ref(mem.toString)
    case LinkToTpl(tpl) => s"""\\hyperref[scaladoc:${tpl}]{${convertInline(el.title)}}"""
    case LinkToExternal(name, url) => handleExternalLink(url, name)
    case Tooltip(name) => name
  }

  /**
   * Generate appropriate section by level.
   * @param i the level (can't be < 1 apparently)
   * @return appropriate Latex command
   */
  def sectionByLevel(i: Int): String = i match {
    case 1 => "\\subsection"
    case 2 => "\\subsubsection"
    case _ => "\\paragraph"
  }

  /** Create reference to some entity. */
  def ref(qualName: String) = s"""\\hyperref[scaladoc:${produceValidLabel(qualName)}]{${convertSpecialChars(qualName)}}"""

  /** Override this to convert code samples in any special way. */
  def handleCodeSample(d: String) = d

  def handleExternalLink(url: String, title: String) = s"\\href{${url}}{${title}}"

  def env(name: String, content: String) = s"\\begin{${name}}\n${content}\n\\end{${name}}\n"

  def item(content: String) = s"\n\\item ${content}\n"
}