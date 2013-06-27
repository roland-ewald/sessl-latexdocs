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

import java.io.File
import java.io.FileWriter
import java.util.Date
import scala.Option.option2Iterable
import scala.tools.nsc.doc.base.comment.Block
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
import scala.tools.nsc.doc.doclet.Generator
import scala.tools.nsc.doc.doclet.Indexer
import scala.tools.nsc.doc.doclet.Universer
import scala.tools.nsc.doc.model.DocTemplateEntity
import com.weiglewilczek.slf4s.Logging
import freemarker.template.Configuration
import freemarker.template.DefaultObjectWrapper
import freemarker.template.Template
import freemarker.template.TemplateException
import java.io.PrintWriter
import scala.tools.nsc.doc.model.Def
import LatexStringConverter.convertSpecialChars
import LatexStringConverter.camelCaseHyphenation
import scala.tools.nsc.doc.base.comment.Body

/**
 * A simple Scaladoc generator for LaTeX.
 *
 * @see [[scala.tools.nsc.doc.html.HtmlFactory]]
 * @see [[scala.tools.nsc.doc.html.page.Template]]
 *
 * @author Roland Ewald
 */
class LatexGenerator extends FreemarkerGenerator {

  type MethodList = java.util.List[DefWrapper]

  override def defaultTemplateFile = "basic_template.tex.fm"

  override def defaultTargetFile = "scaladocs.tex"

  override def wrap(d: DocTemplateEntity) = LatexWrapper(d)

  trait CommentedEntity {

    def comm: Option[Comment]

    def comment: String = convertComment(comm)
    def summary: String = convertSummary(comm)

    def examples = listFromComment(comm)(_.example.map(ExampleWrapper(_)))
    def typeParams = listFromComment(comm)(_.typeParams.map(x => ParamWrapper(x._1, x._2)))
    def valueParams = listFromComment(comm)(_.valueParams.map(x => ParamWrapper(x._1, x._2)))

    def hasExamples = !examples.isEmpty
    def hasTypeParams = !typeParams.isEmpty
    def hasValueParams = !valueParams.isEmpty

    private def listFromComment[X](c: Option[Comment])(f: Comment => Iterable[X]): java.util.List[X] =
      toJavaList(c.map(f).getOrElse(Nil))
  }

  case class LatexWrapper(doc: DocTemplateEntity) extends CommentedEntity {
    def comm = doc.comment
    def name: String = convertSpecialChars(doc.qualifiedName)
    def allMethods: MethodList = retrieveMethods(_ => true)
    def methods: MethodList = retrieveMethods(d => d.inDefinitionTemplates.head == d.inTemplate || d.isConstructor)
    private def retrieveMethods(f: Def => Boolean): MethodList = toJavaList(doc.methods.filter(f).map(DefWrapper(_)))
  }

  case class DefWrapper(d: Def) extends CommentedEntity {
    def comm = d.comment
    def name: String = camelCaseHyphenation(convertSpecialChars(d.name))
  }

  case class ExampleWrapper(body: Body) {
    def text: String = (body.blocks collect { case c: Code => c } map convertBlock).mkString
  }

  case class ParamWrapper(originalName: String, body: Body) {
    def name: String = convertSpecialChars(originalName)
    def comment: String = convertCommentBody(body)
    def summary: String = body.summary.map(convertInline).getOrElse("")
  }

  def convertSummary(co: Option[Comment]): String =
    (for (c <- co; sum <- c.body.summary) yield convertInline(sum)).getOrElse("")

  def convertComment(c: Option[Comment]): String =
    c.map(x => convertCommentBody(x.body)).getOrElse("")

  def convertCommentBody(b: Body): String =
    b.blocks.map(convertBlock).mkString

  def convertInline(i: Inline): String = i match {
    case c: Chain => c.items.map(convertInline).mkString("")
    case t: Text => convertSpecialChars(t.text)
    case u: Underline => s"\\underline{${convertInline(u.text)}}"
    case b: Bold => s"\\textbf{${convertInline(b.text)}}"
    case i: Italic => s"\\emph{${convertInline(i.text)}}"
    case s: Superscript => s"\\textsuperscript{${convertInline(s.text)}}"
    case s: Subscript => s"\\textsubscript{${convertInline(s.text)}}"
    case l: Link => s"\\href{${l.target}}{${convertInline(l.title)}}"
    case m: Monospace => s"\\texttt{${convertInline(m.text)}}"
    case el: EntityLink => s"\\url{${convertInline(el.title)}}" //TODO: use references
    case html: HtmlTag =>
      logger.warn("Ignoring HTML tag '" + html + "'"); ""
    case sum: Summary => convertInline(sum.text)
  }

  def convertBlock(b: Block): String = b match {
    case t: Title => sectionByLevel(t.level) + "{" + convertInline(t.text) + "}\n\n"
    case p: Paragraph => "\n" + convertInline(p.text) + "\n"
    case c: Code => handleCodeSample(c.data)
    case ul: UnorderedList => env("itemize", ul.items.map(convertBlock).map(item).mkString)
    case ol: OrderedList => env("enumerate", ol.items.map(convertBlock).map(item).mkString)
    case dl: DefinitionList => env("itemize", dl.items.map(x =>
      "\\textbf{" + convertInline(x._1) + "}:" + convertBlock(x._2)).map(item).mkString)
    case h: HorizontalRule => "\n\\hline\n"
  }

  def sectionByLevel(i: Int): String = i match {
    case 1 => "\\subsection"
    case 2 => "\\subsubsection"
    case _ => "\\paragraph"
  }

  /** Override this to convert special chars etc. */
  def handleCodeSample(d: String) = d

  def env(name: String, content: String) = s"\\begin{${name}}\n${content}\n\\end{${name}}\n"

  def item(content: String) = s"\n\\item ${content}\n"

}