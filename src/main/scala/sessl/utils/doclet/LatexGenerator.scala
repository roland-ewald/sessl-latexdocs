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
import freemarker.template.TemplateException
import java.util.Date
import freemarker.template.Template

/**
 * A simple Scaladoc generator for LaTeX.
 *
 * @see [[scala.tools.nsc.doc.html.HtmlFactory]]
 * @see [[scala.tools.nsc.doc.html.page.Template]]
 *
 * @author Roland Ewald
 */
class LatexGenerator extends Generator with Universer with Indexer with Logging {

  type QualifiedName = String

  type ElementDescription = (QualifiedName, String)

  type IncludeFilter = DocTemplateEntity => Boolean

  val influcdeAllTypes: IncludeFilter = _.isType

  val templateDirectory = "/"

  val templateFile = "basic_template.tex.fm"

  val targetFile = "scaladocs.tex"

  def targetDir: File = new File(universe.settings.outdir.value)

  override def generateImpl() {
    logger.info(s"Scaladoc LaTeX Generator\nTarget: ${targetDir}")
    val eDescs = generateDescriptions(universe.rootPackage, _.isClass)
    fillTemplate(createTemplateData(eDescs))
  }

  def createTemplateData(e: List[ElementDescription]): Map[String, Any] = {
    Map[String, Any]("date" -> new Date().toString)
  }

  def fillTemplate(data: Map[String, Any]) {
    var out: FileWriter = null
    try {
      out = new FileWriter(targetDir.getAbsolutePath() +
        File.separatorChar + targetFile)
      configureTemplate().process(toJavaMap(data), out)
    } catch {
      case ex: TemplateException => logger.error("Error processing template file.", ex)
    } finally {
      out.close()
    }
  }

  def toJavaMap(m: Map[String, Any]): java.util.HashMap[String, Any] = {
    val jm = new java.util.HashMap[String, Any]()
    m.foreach(x => jm.put(x._1, x._2))
    jm
  }

  /**
   * Walk through the entity tree.
   *  @param dte the entity
   *  @param include method to specify which entities to include (default is all)
   */
  def generateDescriptions(dte: DocTemplateEntity, include: IncludeFilter = influcdeAllTypes): List[ElementDescription] = {
    val elems = dte.templates collect { case x: DocTemplateEntity => x } flatMap (generateDescriptions(_, include))
    if (include(dte))
      toLatex(dte) :: elems
    else
      elems
  }

  /** Convert type description to LaTeX. */
  def toLatex(dte: DocTemplateEntity): ElementDescription = {
    logger.info(s"Generating description of ${dte.qualifiedName}")
    val description =
      "\\section{" + dte.name + "}\n\\label{scaladoc:" + dte.qualifiedName + "}" +
        dte.comment.map(convertComment).mkString
    //TODO: finish this...
    (dte.qualifiedName, description)
  }

  def configureTemplate(): Template = {
    val cfg = new Configuration()
    cfg.setClassForTemplateLoading(classOf[LatexGenerator], templateDirectory)
    cfg.setObjectWrapper(new DefaultObjectWrapper())
    cfg.getTemplate(templateFile)
  }

  def convertComment(c: Comment): String = {
    c.body.blocks.map(convertBlock).mkString
  }

  def convertInline(i: Inline): String = i match {
    case c: Chain => c.items.map(convertInline).mkString("")
    case t: Text => t.text
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
    case p: Paragraph => "\n\n" + convertInline(p.text) + "\n\n"
    case c: Code => env("verbatim", c.data) //TODO: use lstlistings
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

  def env(name: String, content: String) = s"\\begin{${name}}\n${content}\n\\end{${name}}\n"

  def item(content: String) = s"\n\\item${content}\n"

}