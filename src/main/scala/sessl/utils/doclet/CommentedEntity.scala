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

import scala.tools.nsc.doc.base.comment.Body
import scala.tools.nsc.doc.base.comment.Code
import scala.tools.nsc.doc.base.comment.Comment
import scala.tools.nsc.doc.model.Def
import scala.tools.nsc.doc.model.DocTemplateEntity

import FreemarkerGenerator.toJavaList
import LatexConverter.convertBlock
import LatexConverter.convertComment
import LatexConverter.convertCommentBody
import LatexConverter.convertInline
import LatexConverter.convertSummary
import LatexStringConverter.camelCaseHyphenation
import LatexStringConverter.codeToInline
import LatexStringConverter.convertSpecialChars
import LatexStringConverter.produceValidLabel

/**
 * Super trait of (most) wrappers for [[DocTemplateEntity]] and its (documented) sub-elements.
 *
 * These wrappers are instantiated by the [[LatexGenerator]], and made available to the template by the [[FreemarkerGenerator]].
 *
 * @author Roland Ewald
 */
sealed trait CommentedEntity {

  /** The raw comment of the entity. */
  def comm: Option[Comment]

  def comment: String = convertComment(comm)
  def summary: String = convertSummary(comm)

  def examples = listFromComment(comm)(_.example.map(ExampleWrapper(_)))
  def typeParams = listFromComment(comm)(_.typeParams.map(x => ParamWrapper(x._1, x._2)))
  def valueParams = listFromComment(comm)(_.valueParams.map(x => ParamWrapper(x._1, x._2)))

  def hasExamples = !examples.isEmpty
  def hasTypeParams = !typeParams.isEmpty
  def hasValueParams = !valueParams.isEmpty

  /**
   * Freemarker requires Java collections, so all returned collections are converted to `java.util.List[X]`.
   *  @param c the comment
   *  @param f function to return elements to be extracted from comment
   */
  private def listFromComment[X](c: Option[Comment])(f: Comment => Iterable[X]): java.util.List[X] =
    toJavaList(c.map(f).getOrElse(Nil))
}

/**
 * Represents the topmost wrapper, this is the only wrapper instantiated by [[LateyGenerator]].
 *  @param doc the wrapped [[DocTemplateEntity]]
 */
case class TypeOrPackageWrapper(doc: DocTemplateEntity) extends CommentedEntity {
  type MethodList = java.util.List[DefWrapper]
  def comm = doc.comment
  def name: String = convertSpecialChars(doc.qualifiedName)
  def allMethods: MethodList = retrieveMethods(_ => true)
  def methods: MethodList = retrieveMethods(d => d.inDefinitionTemplates.head == d.inTemplate || d.isConstructor)
  private def retrieveMethods(f: Def => Boolean): MethodList = toJavaList(doc.methods.filter(f).map(DefWrapper(_)))
}

/** Wraps a method definition. */
case class DefWrapper(d: Def) extends CommentedEntity {
  def comm = d.comment
  def name: String = camelCaseHyphenation(convertSpecialChars(d.name))
  def qualifiedName = d.qualifiedName
  def labelName = produceValidLabel(d.qualifiedName)
}

/** Wraps example code. */
case class ExampleWrapper(body: Body) {
  def code: String = (body.blocks collect { case c: Code => c } map convertBlock).mkString
  def inlineCode: String = codeToInline(code)
}

/** Wraps a (type) parameter. */
case class ParamWrapper(originalName: String, body: Body) {
  def name: String = convertSpecialChars(originalName)
  def comment: String = convertCommentBody(body)
  def summary: String = body.summary.map(convertInline).getOrElse("")
}