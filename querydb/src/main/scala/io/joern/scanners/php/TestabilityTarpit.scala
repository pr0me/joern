package io.joern.scanners.php

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

  @q
  def autoloadingClassesUsed(): Query =
    Query.make(
      name = "autoload-classes",
      author = Crew.lukas,
      title = "Tarpit: Use PHP autoloading functionality",
      description = """
            | PHP offers a feature for auto loading the classes instead of including them in each file. 
            | Translating the class name to the file name follows certain rules:
            | Each level of the hierarchy is separated with a single underscore. 
            | Class names will directly map to the directories in which they are stored.
            | A class named `Simplarity_Plugin` would be defined in the file src/Simplarity/Plugin.php.
            | For some static analyzers, it is difficult to find the class file.
            |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        cpg.call(".*INIT_FCALL.*").argument.order(2).code("spl_autoload_register")
      }),
      codeExamples = CodeExamples(
        List("""
            |
            |<?php
            |spl_autoload_register(function ($class_name) {
            |    include './'. $class_name . '.php';
            |});
            |$a = $_GET["p1"];
            |$obj  = new MyClass1($a);
            |
            |<?php
            |class MyClass1{
            |    function __construct($b){
            |        echo $b;
            |    }
            |}
            |""".stripMargin),
        List("""
            |
            |""".stripMargin)
      ),
      tags = List(QueryTags.testabilityTarpit, QueryTags.xss)
    )

  @q
  def callOverloaded(): Query =
    Query.make(
      name = "call-overloading",
      author = Crew.lukas,
      title = "Tarpit: Overloading an object's `__call` function",
      description = """
            | By overloading the `__call` and `__callStatic` magic functions, 
            | usually triggered when invoking inaccessible methods, an argument context is created which is hard
            | to reason about for a static analyzer. 
            |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        def methods1 =
          cpg.typeDecl.filter { x => x.method.name.l.contains("__call") }.name.l

        cpg.call("NEW").argument.filter { x => methods1.contains(x.code.toLowerCase) }
      }),
      codeExamples = CodeExamples(
        List("""
            |
            |<?php
            |class MethodTest
            |{
            |    public function __call($name, $arguments)
            |    {
            |        foreach($arguments as $arg){
            |            echo $arg . "\n";
            |        }
            |    }
            |}
            |
            |$b = $_GET["p1"];
            |$obj->x = $b;
            |$obj = new MethodTest;
            |
            |// Will call the __call() function
            |// and print the argument $b => XSS 
            |$obj->runTest('arg1',$b);
            |""".stripMargin),
        List("""
            |
            |""".stripMargin)
      ),
      tags = List(QueryTags.testabilityTarpit, QueryTags.xss)
    )
}
