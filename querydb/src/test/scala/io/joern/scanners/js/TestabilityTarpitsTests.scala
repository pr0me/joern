package io.joern.scanners.js

import io.joern.suites.JSQueryTestSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language._
import io.joern.console.scan._

class TestabilityTarpitsTests extends JSQueryTestSuite {
  override def queryBundle = TestabilityTarpit

  "find the passing of a function as an argument" in {
    queryBundle.functionAsArg()(cpg).map(_.evidence) match {
      case List(IndexedSeq(expr: nodes.Expression)) =>
        expr.code shouldBe "print(n, MyFunction)"
      case _ => fail()
    }
  }
}
