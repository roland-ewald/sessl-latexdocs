SESSL utilities: Scaladocs Generator for LaTeX
==============================================

Tested with: 
- Scala 2.10.2 (http://scala-lang.org)
- Maven 3 (http://maven.apache.org)
- MikTeX 2.9 (http://miktex.org/)

How to Use
==========

After compilation with "mvn test-compile", execute from command line:

scala -classpath "[CLASSPATH]" scala.tools.nsc.ScalaDoc -d someFolder -doc-generator sessl.utils.doclet.LatexGenerator path/to/File1.scala path/to/File2.scala ...

You can create your local CLASSPATH with "mvn dependency:build-classpath" (there are probably nicer ways to execute this).

Make sure the Scala compiler jar is also on the classpath, and that 'someFolder' already exists.

License: Apache 2.0

How to Change
=============

The LaTeX file is generated with Freemarker (http://freemarker.org), from the default template at src/main/resources/basic_template.tex.fm.

You can use your own template by setting the property sessl.utils.doclet.template.

Most of the Latex generation is handled in src/main/scala/sessl/utils/doclet/LatexConverter.scala.