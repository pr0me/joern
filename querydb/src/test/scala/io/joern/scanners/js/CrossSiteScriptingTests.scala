package io.joern.scanners.js

import io.joern.suites.JSQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language._
import io.joern.console.scan._

class CrossSiteScriptingTests extends JSQueryTestSuite {
  override def queryBundle = CrossSiteScripting

  "find reflected XSS in NodeJS" in {
    queryBundle.reflectdXSS()(cpg).map(_.evidence) match {
      case List(IndexedSeq(expr: nodes.Expression)) =>
        expr.code shouldBe "res.write(message)"
      case _ => fail()
    }
  }
}