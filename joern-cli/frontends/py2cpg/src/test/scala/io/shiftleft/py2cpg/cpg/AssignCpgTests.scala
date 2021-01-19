package io.shiftleft.py2cpg.cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, nodes}
import io.shiftleft.py2cpg.Py2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AssignCpgTests extends AnyFreeSpec with Matchers {
  "single target assign" - {
    lazy val cpg = Py2CpgTestContext.buildCpg(
      """x = 2""".stripMargin
    )

    "test single target assign call node properties" in {
      val assignCall = cpg.call.methodFullName(Operators.assignment).head
      assignCall.code shouldBe "x = 2"
      assignCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignCall.lineNumber shouldBe Some(1)
      assignCall.columnNumber shouldBe Some(3)
    }

    "test single target assign call ast children" in {
      cpg.call.methodFullName(Operators.assignment).astChildren.order(1).isIdentifier.head.code shouldBe "x"
      cpg.call.methodFullName(Operators.assignment).astChildren.order(2).isLiteral.head.code shouldBe "2"
    }

    "test single target assign call arguments" in {
      cpg.call.methodFullName(Operators.assignment).argument.argumentIndex(1).isIdentifier.head.code shouldBe "x"
      cpg.call.methodFullName(Operators.assignment).argument.argumentIndex(2).isLiteral.head.code shouldBe "2"
    }
  }

  "multi target assign" - {
    // Multi target assign statements get lowered to a block with
    // a local variable for the right hand side and an assignment
    // inside the block for each element in the target list.
    lazy val cpg = Py2CpgTestContext.buildCpg(
      """x, y = list""".stripMargin
    )

    "test multi target assign block node properties" in {
      val block = cpg.all.collect { case block: nodes.Block => block}.head
      block.lineNumber shouldBe Some(1)
    }

    "test multi target assign local node" in {
      val block = cpg.all.collect { case block: nodes.Block => block}.head
      block.astChildren.order(1).isLocal.head.code shouldBe "tmp"
    }

    "test multi target assign tmp variable assignment" in {
      val block = cpg.all.collect { case block: nodes.Block => block}.head
      block.astChildren.order(2).isCall.head.code shouldBe "tmp = list"
    }

    "test multi target assign block ast children" in {
      val block = cpg.all.collect { case block: nodes.Block => block}.head
      block.astChildren.order(3).isCall.head.code shouldBe "x = tmp[0]"
      block.astChildren.order(4).isCall.head.code shouldBe "y = tmp[1]"
    }

    "test multi target assign lowering to assignment for variable x" in {
      val assignCall = cpg.call.methodFullName(Operators.assignment).order(3).head
      assignCall.code shouldBe "x = tmp[0]"
      assignCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignCall.lineNumber shouldBe Some(1)

      assignCall.argument(1).code shouldBe "x"
      assignCall.argument(2).code shouldBe "tmp[0]"
    }
  }

  "nested multi target assign" - {
    // Nested multi target assign statements get lowered to a block with
    // a local variable for the right hand side, an assignment of the
    // right hand side to the local and an assignment
    // for each element in the target list.
    lazy val cpg = Py2CpgTestContext.buildCpg(
      """x, (y, z) = list""".stripMargin
    )

    "test multi target assign block node properties" in {
      val block = cpg.all.collect { case block: nodes.Block => block}.head
      block.lineNumber shouldBe Some(1)
    }

    "test multi target assign local node" in {
      val block = cpg.all.collect { case block: nodes.Block => block}.head
      block.astChildren.order(1).isLocal.head.code shouldBe "tmp"
    }

    "test multi target assign tmp variable assignment" in {
      val block = cpg.all.collect { case block: nodes.Block => block}.head
      block.astChildren.order(2).isCall.head.code shouldBe "tmp = list"
    }

    "test multi target assign block ast children" in {
      val block = cpg.all.collect { case block: nodes.Block => block}.head
      block.astChildren.order(3).isCall.head.code shouldBe "x = tmp[0]"
      block.astChildren.order(4).isCall.head.code shouldBe "y = tmp[1][0]"
    }

    "test multi target assign lowering to assignment for variable y" in {
      val assignCall = cpg.call.methodFullName(Operators.assignment).order(4).head
      assignCall.code shouldBe "y = tmp[1][0]"
      assignCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignCall.lineNumber shouldBe Some(1)

      assignCall.argument(1).code shouldBe "y"
      assignCall.argument(2).code shouldBe "tmp[1][0]"
    }
  }
}
