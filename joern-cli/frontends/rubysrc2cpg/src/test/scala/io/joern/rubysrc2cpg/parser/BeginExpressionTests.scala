package io.joern.rubysrc2cpg.parser

class BeginExpressionTests extends RubyParserAbstractTest {

  "A begin expression" should {

    "be parsed as a primary expression" when {

      "it contains a `rescue` clause with both exception class and exception variable" in {
        val code = """begin
            |1/0
            |rescue ZeroDivisionError => e
            |end""".stripMargin

        printAst(_.primary(), code) shouldBe
          """BeginExpressionPrimary
            | BeginExpression
            |  begin
            |  WsOrNl
            |  BodyStatement
            |   CompoundStatement
            |    Statements
            |     ExpressionOrCommandStatement
            |      ExpressionExpressionOrCommand
            |       MultiplicativeExpression
            |        PrimaryExpression
            |         LiteralPrimary
            |          NumericLiteralLiteral
            |           NumericLiteral
            |            UnsignedNumericLiteral
            |             1
            |        /
            |        PrimaryExpression
            |         LiteralPrimary
            |          NumericLiteralLiteral
            |           NumericLiteral
            |            UnsignedNumericLiteral
            |             0
            |    Separators
            |     Separator
            |   RescueClause
            |    rescue
            |    ExceptionClass
            |     PrimaryExpression
            |      VariableReferencePrimary
            |       VariableIdentifierVariableReference
            |        VariableIdentifier
            |         ZeroDivisionError
            |    WsOrNl
            |    ExceptionVariableAssignment
            |     =>
            |     VariableIdentifierOnlySingleLeftHandSide
            |      VariableIdentifier
            |       e
            |    ThenClause
            |     Separator
            |     CompoundStatement
            |  end""".stripMargin
      }
    }
  }

}
