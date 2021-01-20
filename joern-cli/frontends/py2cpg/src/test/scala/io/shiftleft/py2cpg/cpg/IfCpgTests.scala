package io.shiftleft.py2cpg.cpg

import io.shiftleft.py2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.nodes
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class IfCpgTests extends AnyFreeSpec with Matchers {
  lazy val cpg = Py2CpgTestContext.buildCpg(
    """if x :
      |  y
      |else:
      |  z""".stripMargin
  )

  "test control structure node properties" in {
    val controlStructureNode = cpg.controlStructure.head
    controlStructureNode.code shouldBe "if ... : ..."
    controlStructureNode.parserTypeName shouldBe "IfStatement"
    controlStructureNode.lineNumber shouldBe Some(1)
  }

  "test control structure condition" in {
    val conditionNode = cpg.controlStructure.condition.head
    conditionNode.code shouldBe "x"
    cpg.controlStructure.astChildren.order(1).head shouldBe conditionNode
  }

  "test control structure body" in {
    val bodyNode = cpg.controlStructure.whenTrue.isExpression.head
    bodyNode.isInstanceOf[nodes.Block] shouldBe true
    bodyNode.code shouldBe "y"
    cpg.controlStructure.astChildren.order(2).head shouldBe bodyNode
  }

  "test control structure else" in {
    val elseNode = cpg.controlStructure.whenFalse.isExpression.head
    elseNode.isInstanceOf[nodes.Block] shouldBe true
    elseNode.code shouldBe "z"
    cpg.controlStructure.astChildren.order(3).head shouldBe elseNode
  }
}
