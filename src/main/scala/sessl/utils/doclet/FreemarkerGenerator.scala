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

import scala.tools.nsc.doc.doclet.Generator
import scala.tools.nsc.doc.doclet.Indexer
import scala.tools.nsc.doc.doclet.Universer
import scala.tools.nsc.doc.model.DocTemplateEntity

import com.weiglewilczek.slf4s.Logging

import freemarker.template.Configuration
import freemarker.template.DefaultObjectWrapper
import freemarker.template.Template
import freemarker.template.TemplateException

/**
 * This is a [[Generator]] that writes the documentation to a single [[http://freemarker.org Freemarker]] template.
 *
 * Template file and target file can be configured with the properties `sessl.utils.doclet.template` and `sessl.utils.doclet.target`, respectively.
 *
 * @author Roland Ewald
 */
abstract class FreemarkerGenerator extends Generator with Universer with Indexer with Logging {

  import FreemarkerGenerator._
  
  /** The default template. */
  def defaultTemplateFile: String

  /** The default name of the file to produce. */
  def defaultTargetFile: String

  /**
   * Create a wrapper for a [[DocTemplateEntity]].
   *  @param d the documented entity
   *  @return a wrapper to generate documentation for this entity, to be called from the freemarker template
   */
  def wrap(d: DocTemplateEntity): Any

  /** Filter which [[DocTemplateEntity]] to consider. */
  def filter(d: DocTemplateEntity): Boolean = true

  /** The directory root from which to retrieve the template file. */
  def templateDirectory = "/"

  /** The template file. */
  def templateFile = sys.props.getOrElse("sessl.utils.doclet.template", defaultTemplateFile)

  /** The target file. */
  def targetFile = sys.props.getOrElse("sessl.utils.doclet.target", defaultTargetFile)

  /** The target directory. */
  def targetDir = new File(universe.settings.outdir.value)

  override def generateImpl(): Unit = {
    logger.info(s"Scaladoc LaTeX Generator\nTarget: ${targetDir}")
    val entities = retrieveEntities(universe.rootPackage, filter)
    logger.info(s"Found ${entities.length} entities:\n${entities.mkString("\n")}")
    fillTemplate(createTemplateData(entities))
  }

  def createTemplateData(entities: Seq[DocTemplateEntity]): Map[String, Any] = {
    val wrappedEntites = entities.sortBy(_.qualifiedName).map(wrap)
    Map[String, Any](
      "templateFile" -> templateFile,
      "targetFile" -> targetFile,
      "date" -> new Date().toString,
      "entities" -> toJavaList(wrappedEntites))
  }

  def fillTemplate(data: Map[String, Any]): Unit = {
    val out = new FileWriter(targetDir.getAbsolutePath() + File.separatorChar + targetFile)
    try {
      configureTemplate().process(toJavaMap(data), out)
    } catch {
      case ex: TemplateException => logger.error("Error processing template file.", ex)
    } finally {
      out.close()
    }
  }

  def configureTemplate(): Template = {
    val cfg = new Configuration
    cfg.setClassForTemplateLoading(classOf[LatexGenerator], templateDirectory)
    cfg.setObjectWrapper(new DefaultObjectWrapper())
    println(templateFile)
    cfg.getTemplate(templateFile)
  }
}

/**
 * Auxiliary methods.
 */
object FreemarkerGenerator {

  /**
   * Walk through the entity tree and retrieve entities to be documented.
   *  @param dte the entity
   *  @param include method to specify which entities to include
   */
  def retrieveEntities(dte: DocTemplateEntity, include: DocTemplateEntity => Boolean): Seq[DocTemplateEntity] = {
    val docEntities = dte.templates collect { case x: DocTemplateEntity => x }
    val childElements = docEntities.flatMap(retrieveEntities(_, include))
    if (include(dte))
      dte :: childElements
    else
      childElements
  }

  def toJavaMap[A, B](m: Map[A, B]): java.util.HashMap[A, B] = {
    val rt = new java.util.HashMap[A, B]()
    for ((k, v) <- m) rt.put(k, v)
    rt
  }

  def toJavaList[A](xs: Iterable[A]): java.util.ArrayList[A] = {
    val rt = new java.util.ArrayList[A]()
    for (x <- xs) rt.add(x)
    rt
  }

}