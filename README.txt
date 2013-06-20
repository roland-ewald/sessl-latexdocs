SESSL utilities: Scaladocs Generator for LaTeX
==============================================

Tested with: 
- Scala 2.10 (http://scala-lang.org)
- Maven 3 (http://maven.apache.org)
- MikTeX 2.9 (http://miktex.org/)

Compile with: mvn test-compile

Usage: 

execute scala.tools.nsc.ScalaDoc with -d someFolder -doc-generator sessl.utils.doclet.LatexGenerator src/test/scala/sessl/utils/doclet/SampleClass.scala

Make sure the scala compiler jar is on the classpath.

License: Apache 2.0