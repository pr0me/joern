package io.joern.suites

import io.joern.util.QueryUtil
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.console.scan._
import io.joern.console.QueryBundle
import io.joern.console.Query
import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.testfixtures.TestCpg

class JSQueryTestSuite extends DataFlowCodeToCpgSuite {
  val argumentProvider = new QDBArgumentProvider(3)

  var cpg: Cpg = _

  override def beforeAll(): Unit = {
    buildCpgForDir(concatedQueryCodeExamples)
    super.beforeAll()
  }

  def queryBundle: QueryBundle = QueryUtil.EmptyBundle

  def allQueries: List[Query] = QueryUtil.allQueries(queryBundle, argumentProvider)

  def concatedQueryCodeExamples: String =
    allQueries
      .map { q =>
        q.codeExamples.positive
          .mkString("\n")
          .concat("\n")
          .concat(
            q.codeExamples.negative
              .mkString("\n")
          )
      }
      .mkString("\n")

  def buildCpgForDir(code: String): Unit = {
    val testCpg = super.code(code)
    cpg = testCpg
  }
}
