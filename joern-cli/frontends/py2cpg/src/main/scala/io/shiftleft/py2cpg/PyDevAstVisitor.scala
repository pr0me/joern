package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, nodes}
import io.shiftleft.passes.DiffGraph
import org.python.pydev.parser.jython.ast

import scala.collection.mutable

object PyDevAstVisitor {
  private implicit class ToNewNodeConverter(node: AnyRef) {
    def cast: nodes.NewNode = {
      node.asInstanceOf[nodes.NewNode]
    }
  }
}

class PyDevAstVisitor extends ast.VisitorIF with PyDevAstVisitorHelpers {
  import PyDevAstVisitor._

  private val diffGraph = new DiffGraph.Builder()
  protected val nodeBuilder = new NodeBuilder(diffGraph)
  protected val edgeBuilder = new EdgeBuilder(diffGraph)

  def getDiffGraph: DiffGraph = {
    diffGraph.build()
  }

  override def visitModule(module: ast.Module): nodes.NewNode = {
    module.traverse(this)
    null
  }

  override def visitInteractive(interactive: ast.Interactive): nodes.NewNode = ???

  override def visitExpression(expression: ast.Expression): nodes.NewNode = ???

  override def visitNameTok(nameTok: ast.NameTok): nodes.NewNode = {
    nodeBuilder.fieldIdentifierNode(nameTok.id, lineAndColOf(nameTok))
  }

  override def visitSuite(suite: ast.Suite): nodes.NewNode = {
    val suiteStmtNodes = suite.body.map(_.accept(this).cast)
    val suiteBlockNode = createBlock(Iterable.empty, suiteStmtNodes, lineAndColOf(suite))
    suiteBlockNode
  }

  override def visitWithItem(withItem: ast.WithItem): nodes.NewNode = ???

  override def visitFunctionDef(functionDef: ast.FunctionDef): nodes.NewNode = ???

  override def visitClassDef(classDef: ast.ClassDef): nodes.NewNode = ???

  override def visitReturn(aReturn: ast.Return): nodes.NewNode = ???

  override def visitDelete(delete: ast.Delete): nodes.NewNode = ???

  override def visitAssign(assign: ast.Assign): nodes.NewNode = {
    if (assign.targets.size == 1) {
      val target = assign.targets(0)
      val targetWithAccessChains = getTargetsWithAccessChains(target)
      if (targetWithAccessChains.size == 1) {
        // Case with single entity one the left hand side.
        // We always have an empty acces chain in this case.
        val valueNode = assign.value.accept(this).cast
        val targetNode = target.accept(this).cast

        createAssignment(targetNode, valueNode, lineAndColOf(assign))
      } else {
        // Case with a list of entities on the left hand side.
        val valueNode = assign.value.accept(this).cast
        val tmpVariableName = getUnusedName()

        val localNode = nodeBuilder.localNode(tmpVariableName)

        val tmpIdentifierNode =
          nodeBuilder.identifierNode(tmpVariableName, lineAndColOf(assign))
        val tmpVariableAssignNode =
          createAssignment(tmpIdentifierNode, valueNode, lineAndColOf(assign))

        val targetAssignNodes =
          targetWithAccessChains.map { case (target, accessChain) =>
            val targetNode = target.accept(this).cast
            val tmpIdentifierNode =
              nodeBuilder.identifierNode(tmpVariableName, lineAndColOf(assign))
            val indexTmpIdentifierNode = createIndexAccessChain(
              tmpIdentifierNode,
              accessChain,
              lineAndColOf(assign)
            )

            createAssignment(
              targetNode,
              indexTmpIdentifierNode,
              lineAndColOf(assign)
            )
          }

        val blockNode =
          createBlock(
            Iterable.single(localNode),
            tmpVariableAssignNode :: targetAssignNodes.toList,
            lineAndColOf(assign)
          )

        blockNode
      }
    } else {
      throw new RuntimeException("Unexpected assign with more than one target.")
    }
  }

  override def visitAugAssign(augAssign: ast.AugAssign): nodes.NewNode = ???

  override def visitPrint(print: ast.Print): nodes.NewNode = ???

  override def visitFor(aFor: ast.For): nodes.NewNode = ???

  override def visitWhile(astWhile: ast.While): nodes.NewNode = {
    val conditionNode = astWhile.test.accept(this).cast
    val bodyStmtNodes = astWhile.body.map(_.accept(this).cast)
    val elseNode = astWhile.orelse.accept(this).cast

    val bodyBlockNode = createBlock(Iterable.empty, bodyStmtNodes, lineAndColOf(astWhile))

    val controlStructureNode =
      nodeBuilder.controlStructureNode("while ... : ...", "WhileStatement", lineAndColOf(astWhile))
    edgeBuilder.conditionEdge(conditionNode, controlStructureNode)
    addAstChildNodes(controlStructureNode, 1, conditionNode, bodyBlockNode, elseNode)

    controlStructureNode
  }

  override def visitIf(astIf: ast.If): nodes.NewNode = {
    val conditionNode = astIf.test.accept(this).cast
    val bodyStmtNodes = astIf.body.map(_.accept(this).cast)
    val elseNode = astIf.orelse.accept(this).cast

    val bodyBlockNode = createBlock(Iterable.empty, bodyStmtNodes, lineAndColOf(astIf))

    val controlStructureNode =
      nodeBuilder.controlStructureNode("if ... : ...", "IfStatement", lineAndColOf(astIf))
    edgeBuilder.conditionEdge(conditionNode, controlStructureNode)
    addAstChildNodes(controlStructureNode, 1, conditionNode, bodyBlockNode, elseNode)

    controlStructureNode
  }

  override def visitWith(`with`: ast.With): nodes.NewNode = ???

  override def visitRaise(raise: ast.Raise): nodes.NewNode = ???

  override def visitTryExcept(tryExcept: ast.TryExcept): nodes.NewNode = ???

  override def visitTryFinally(tryFinally: ast.TryFinally): nodes.NewNode = ???

  override def visitAssert(anAssert: ast.Assert): nodes.NewNode = ???

  override def visitImport(anImport: ast.Import): nodes.NewNode = ???

  override def visitImportFrom(importFrom: ast.ImportFrom): nodes.NewNode = ???

  override def visitExec(exec: ast.Exec): nodes.NewNode = ???

  override def visitGlobal(global: ast.Global): nodes.NewNode = ???

  override def visitNonLocal(nonLocal: ast.NonLocal): nodes.NewNode = ???

  override def visitExpr(expr: ast.Expr): nodes.NewNode = {
    expr.value.accept(this).cast
  }

  override def visitPass(pass: ast.Pass): nodes.NewNode = {
    nodeBuilder.callNode(
      "pass",
      "<operator>.pass",
      DispatchTypes.STATIC_DISPATCH,
      lineAndColOf(pass)
    )
  }

  override def visitBreak(astBreak: ast.Break): nodes.NewNode = {
    nodeBuilder.controlStructureNode("break", "BreakStatement", lineAndColOf(astBreak))
  }

  override def visitContinue(astContinue: ast.Continue): nodes.NewNode = {
    nodeBuilder.controlStructureNode("continue", "ContinueStatement", lineAndColOf(astContinue))
  }

  override def visitBoolOp(boolOp: ast.BoolOp): nodes.NewNode = {
    val argNodes = boolOp.values.map(_.accept(this).cast)

    val (operatorCode, methodFullName) =
      boolOp.op match {
        case ast.boolopType.And => (" and ", Operators.logicalAnd)
        case ast.boolopType.Or  => (" or ", Operators.logicalOr)
      }

    val code = argNodes.map(argNode => codeOf(argNode)).mkString(operatorCode)
    val callNode = nodeBuilder.callNode(
      code,
      methodFullName,
      DispatchTypes.STATIC_DISPATCH,
      lineAndColOf(boolOp)
    )

    addAstChildrenAsArguments(callNode, 1, argNodes)

    callNode
  }

  override def visitNamedExpr(namedExpr: ast.NamedExpr): nodes.NewNode = ???

  override def visitBinOp(binOp: ast.BinOp): nodes.NewNode = {
    val lhsNode = binOp.left.accept(this).cast
    val rhsNode = binOp.right.accept(this).cast

    val (operatorCode, methodFullName) =
      binOp.op match {
        case ast.operatorType.Add    => (" + ", Operators.addition)
        case ast.operatorType.Sub    => (" - ", Operators.subtraction)
        case ast.operatorType.Mult   => (" * ", Operators.multiplication)
        case ast.operatorType.Div    => (" / ", Operators.division)
        case ast.operatorType.Mod    => (" % ", Operators.modulo)
        case ast.operatorType.Pow    => (" ** ", Operators.exponentiation)
        case ast.operatorType.LShift => (" << ", Operators.shiftLeft)
        case ast.operatorType.RShift => (" << ", Operators.arithmeticShiftRight)
        case ast.operatorType.BitOr  => (" | ", Operators.or)
        case ast.operatorType.BitXor => (" ^ ", Operators.xor)
        case ast.operatorType.BitAnd => (" & ", Operators.and)
        case ast.operatorType.FloorDiv =>
          (" // ", "<operator>.floorDiv") // TODO make this a define and add policy for this
      }

    val code = codeOf(lhsNode) + operatorCode + codeOf(rhsNode)
    val callNode = nodeBuilder.callNode(
      code,
      methodFullName,
      DispatchTypes.STATIC_DISPATCH,
      lineAndColOf(binOp)
    )

    addAstChildrenAsArguments(callNode, 1, lhsNode, rhsNode)

    callNode
  }

  override def visitUnaryOp(unaryOp: ast.UnaryOp): nodes.NewNode = {
    val operandNode = unaryOp.operand.accept(this).cast

    val (operatorCode, methodFullName) =
      unaryOp.op match {
        case ast.unaryopType.Invert => ("~", Operators.not)
        case ast.unaryopType.Not    => ("not ", Operators.logicalNot)
        case ast.unaryopType.UAdd   => ("+", Operators.plus)
        case ast.unaryopType.USub   => ("-", Operators.minus)
      }

    val code = operatorCode + codeOf(operandNode)
    val callNode = nodeBuilder.callNode(
      code,
      methodFullName,
      DispatchTypes.STATIC_DISPATCH,
      lineAndColOf(unaryOp)
    )

    addAstChildrenAsArguments(callNode, 1, operandNode)

    callNode
  }

  override def visitLambda(lambda: ast.Lambda): nodes.NewNode = ???

  override def visitIfExp(ifExp: ast.IfExp): nodes.NewNode = ???

  override def visitDict(dict: ast.Dict): nodes.NewNode = ???

  override def visitSet(set: ast.Set): nodes.NewNode = ???

  override def visitListComp(listComp: ast.ListComp): nodes.NewNode = ???

  override def visitSetComp(setComp: ast.SetComp): nodes.NewNode = ???

  override def visitDictComp(dictComp: ast.DictComp): nodes.NewNode = ???

  override def visitGeneratorExp(generatorExp: ast.GeneratorExp): nodes.NewNode = ???

  override def visitYield(`yield`: ast.Yield): nodes.NewNode = ???

  override def visitAwait(await: ast.Await): nodes.NewNode = ???

  override def visitCompare(compare: ast.Compare): nodes.NewNode = ???

  /** TODO
    * For now this function compromises on the correctness of the
    * lowering in order to get some data flow tracking going.
    * 1. For constructs like x.func() we assume x to be the
    *    instance which is passed into func. This is not true
    *    since the instance method object gets the instance
    *    already bound/captured during function access.
    *    This becomes relevant for constructs like:
    *    x.func = y.func <- y.func is class method object
    *    x.func()
    *    In this case the instance passed into func is y and
    *    not x. We cannot represent this in th CPG and thus
    *    stick to the assumption that the part before the "."
    *    and the bound/captured instance will be the same.
    *    For reference see:
    *    https://docs.python.org/3/reference/datamodel.html#the-standard-type-hierarchy
    *    search for "Instance methods"
    *
    * 2. Due to the decision in 1. for calls like x.func() the
    *    expression x is part of the call receiver AST and its
    *    instance AST. This would be only ok if x is side effect
    *    free which is not necessarily the case if x == getX().
    *    Currently we ignore this fact and just emit the expression
    *    twice. A fix would mean to emit a tmp variable which holds
    *    the expression result.
    *    Not yet implemented because this gets obsolete if 1. is
    *    fixed.
    * 3. No named parameter support. CPG does not supports this.
    */
  override def visitCall(call: ast.Call): nodes.NewNode = {
    val argumentNodes = call.args.map(_.accept(this).cast)
    val receiverNode = call.func.accept(this).cast

    call.func match {
      case attribute: ast.Attribute =>
        val instanceNode = attribute.value.accept(this).cast
        createInstanceCall(receiverNode, instanceNode, lineAndColOf(call), argumentNodes: _*)
      case _ =>
        createCall(receiverNode, lineAndColOf(call), argumentNodes: _*)
    }
  }

  override def visitRepr(repr: ast.Repr): nodes.NewNode = ???

  override def visitNum(num: ast.Num): nodes.NewNode = {
    nodeBuilder.numberLiteralNode(num.num, lineAndColOf(num))
  }

  override def visitStr(str: ast.Str): nodes.NewNode = {
    nodeBuilder.stringLiteralNode(str.s, lineAndColOf(str))
  }

  override def visitStrJoin(strJoin: ast.StrJoin): nodes.NewNode = ???

  /** TODO
    * We currently ignore possible attribute access provider/interception
    * mechanisms like __getattr__, __getattribute__ and __get__.
    */
  override def visitAttribute(attribute: ast.Attribute): nodes.NewNode = {
    val baseNode = attribute.value.accept(this).cast
    val fieldIdNode = attribute.attr.accept(this).cast

    createFieldAccess(baseNode, fieldIdNode, lineAndColOf(attribute))
  }

  override def visitSubscript(subscript: ast.Subscript): nodes.NewNode = ???

  override def visitStarred(starred: ast.Starred): nodes.NewNode = ???

  override def visitName(name: ast.Name): nodes.NewNode = {
    nodeBuilder.identifierNode(name.id, lineAndColOf(name))
  }

  /**
    * Lowering of [1, 2]:
    *   {
    *     tmp = list
    *     tmp.append(1)
    *     tmp.append(2)
    *     tmp
    *   }
    */
  override def visitList(list: ast.List): nodes.NewNode = {
    val tmpVariableName = getUnusedName()
    val localNode = nodeBuilder.localNode(tmpVariableName)

    val listInstanceId = nodeBuilder.identifierNode(tmpVariableName, lineAndColOf(list))
    val listIdNode = nodeBuilder.identifierNode("list", lineAndColOf(list))
    val listConstructorCall = createCall(listIdNode, lineAndColOf(list))
    val listInstanceAssignment = createAssignment(listInstanceId, listConstructorCall, lineAndColOf(list))

    val appendCallNodes = list.elts.map { listElement =>
      val listInstanceIdForReceiver = nodeBuilder.identifierNode(tmpVariableName, lineAndColOf(list))
      val appendFieldAccessNode = createFieldAccess(listInstanceIdForReceiver, "append", lineAndColOf(list))

      val listeInstanceId = nodeBuilder.identifierNode(tmpVariableName, lineAndColOf(list))
      val elementNode = listElement.accept(this).cast
      createInstanceCall(appendFieldAccessNode, listeInstanceId, lineAndColOf(list), elementNode)
    }

    val listInstanceIdForReturn = nodeBuilder.identifierNode(tmpVariableName, lineAndColOf(list))

    val blockElements = mutable.ArrayBuffer.empty[nodes.NewNode]
    blockElements.append(listInstanceAssignment)
    blockElements.appendAll(appendCallNodes)
    blockElements.append(listInstanceIdForReturn)
    createBlock(Iterable.single(localNode), blockElements, lineAndColOf(list))
  }

  override def visitTuple(tuple: ast.Tuple): nodes.NewNode = ???

  override def visitEllipsis(ellipsis: ast.Ellipsis): nodes.NewNode = ???

  override def visitSlice(slice: ast.Slice): nodes.NewNode = ???

  override def visitExtSlice(extSlice: ast.ExtSlice): nodes.NewNode = ???

  override def visitIndex(index: ast.Index): nodes.NewNode = ???

  override def visitComprehension(comprehension: ast.Comprehension): nodes.NewNode = ???
}
