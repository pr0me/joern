package io.joern.scanners.c

import io.joern.scanners._
import io.joern.console._
import io.joern.macros.QueryMacros._
import io.shiftleft.semanticcpg.language._

object TestabilityTarpit extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def staticVarUsed(): Query =
    Query.make(
      name = "static-variables",
      author = Crew.lukas,
      title = "Tarpit: Static Variable Use",
      description = """
            | Static variables are challenges for vulnerability scanners, because the scanner has to record the last 
            | value for the variable with the last call for the function.
            |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        cpg.call(".*BIND_STATIC.*")
      }),
      codeExamples = CodeExamples(
        List("""
            |
            |<?php
            |
            |function F($a){
            |    static $b = 'abc';
            |    echo $b;
            |    $b = $a;
            |}
            |
            |$a = $_GET["p1"];
            |F($a);
            |F('abc');
            |""".stripMargin),
        List("""
            |
            |""".stripMargin)
      ),
      tags = List(QueryTags.testabilityTarpit, QueryTags.xss)
    )

  @q
  def funcGetArgsUsed(): Query =
    Query.make(
      name = "get-arguments",
      author = Crew.lukas,
      title = "Tarpit: Use of func_get_args for Variadic Functions",
      description = """
            | It is possible to send more arguments to a function than specified. 
            | Then the function `func_get_args` will return an array with the rest of the arguments.
            | As this is highly PHP-specific, many scanners will not be able to reason about such arguments.
            |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        cpg.call(".*FUNC_GET_ARGS.*")
      }),
      codeExamples = CodeExamples(
        List("""
            |
            |<?php
            |function sum() {
            |    // it will print all the parameters
            |    // XSS vulnerability with the last element $b
            |    foreach (func_get_args() as $n) {
            |        echo $n;
            |    }
            |}
            |
            |$b = $_GET["p1"];
            |sum(1, 2, 3, $b);
            |""".stripMargin),
        List("""
            |
            |""".stripMargin)
      ),
      tags = List(QueryTags.testabilityTarpit, QueryTags.xss)
    )
}
