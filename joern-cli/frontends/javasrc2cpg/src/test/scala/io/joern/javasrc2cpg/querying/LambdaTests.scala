package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.edges.{Binds, Capture, Ref}
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{
  Binding,
  Call,
  ClosureBinding,
  Identifier,
  Local,
  MethodParameterIn,
  MethodRef,
  Return,
  TypeDecl
}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.jIteratortoTraversal

class LambdaTests extends JavaSrcCode2CpgFixture {

  "lambdas used as a function argument" should {
    val cpg = code("""
        |import java.util.function.Function;
        |
        |public class Foo {
        |  public static String getFromSupplier(String input, Function<String, String> mapper) {
        |    return mapper.apply(input);
        |  }
        |
        |  public void test1(String input, String fallback) {
        |    getFromSupplier(
        |      input,
        |      lambdaInput -> lambdaInput.length() > 5 ? "Long" : fallback
        |    );
        |  }
        |}
        |""".stripMargin)

    "create a method node for the lambda" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").l match {
        case List(lambdaMethod) =>
          // Lambda body creation tested separately
          lambdaMethod.name shouldBe "lambda$0"
          lambdaMethod.fullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
          lambdaMethod.parameter.l match {
            case List(lambdaInput) =>
              lambdaInput.name shouldBe "lambdaInput"
              lambdaInput.typeFullName shouldBe "java.lang.String"

            case result => fail(s"Expected single lambda parameter but got $result")
          }
          lambdaMethod.methodReturn.typeFullName shouldBe "java.lang.String"

        case result => fail(s"Expected single lambda method but got $result")
      }
    }

    "create the correct binding for the lambda method" in {
      cpg.all.collectAll[Binding].filter(_.name == "lambda$0").l match {
        case List(lambdaBinding) =>
          lambdaBinding.methodFullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
          lambdaBinding.signature shouldBe "java.lang.String(java.lang.String)"

          lambdaBinding.inE.collectAll[Binds].map(_.outNode()).l match {
            case List(typeDecl: TypeDecl) =>
              typeDecl.fullName shouldBe "Foo"

            case result => fail(s"Expected single typeDecl but got $result")
          }

        case result => fail(s"Expected single binding for lambda method but got $result")
      }
    }

    "create a method body for the lambda method" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").block.astChildren.l match {
        case List(_: Local, _: Local, returnNode: Return) =>
          returnNode.code shouldBe "return lambdaInput.length() > 5 ? \"Long\" : fallback;"
          returnNode.astChildren.l match {
            case List(expr: Call) =>
              expr.methodFullName shouldBe Operators.conditional

            case result => fail(s"Expected return conditional, but got $result")
          }

        case result => fail(s"Expected lambda body with single return but got $result")
      }
    }

    "create locals for captured identifiers in the lambda method" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").local.l match {
        case List(fallbackLocal: Local, inputLocal: Local) =>
          inputLocal.name shouldBe "input"
          inputLocal.code shouldBe "input"
          inputLocal.typeFullName shouldBe "java.lang.String"

          fallbackLocal.name shouldBe "fallback"
          fallbackLocal.code shouldBe "fallback"
          fallbackLocal.typeFullName shouldBe "java.lang.String"

        case result => fail(s"Expected single local for fallback but got $result")
      }
    }

    "create closure bindings for captured identifiers" in {
      cpg.all.collectAll[ClosureBinding].l match {
        case List(fallbackClosureBinding, _) =>
          fallbackClosureBinding.label shouldBe "CLOSURE_BINDING"

          val fallbackLocal = cpg.method.name(".*lambda.*").local.name("fallback").head
          fallbackClosureBinding.closureBindingId shouldBe fallbackLocal.closureBindingId

          fallbackClosureBinding.outE.collectAll[Ref].map(_.inNode()).l match {
            case List(capturedParam: MethodParameterIn) =>
              capturedParam.name shouldBe "fallback"
              capturedParam.method.head.fullName shouldBe "Foo.test1:void(java.lang.String,java.lang.String)"
            case result => fail(s"Expected single capturedParam but got $result")
          }

          fallbackClosureBinding.inE.collectAll[Capture].map(_.outNode()).l match {
            case List(outMethod: MethodRef) =>
              outMethod.methodFullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
            case result => fail(s"Expected single METHOD_REF but got $result")
          }

        case result => fail(s"Expected 2 closure bindings for captured variables but got $result")
      }
    }

    "create a typeDecl node inheriting from correct interface" in {
      cpg.typeDecl.name(".*lambda.*").l match {
        case List(lambdaDecl) =>
          lambdaDecl.name shouldBe "lambda$0"
          lambdaDecl.fullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
          lambdaDecl.inheritsFromTypeFullName should contain theSameElementsAs List("java.util.function.Function")

        case result => fail(s"Expected a single typeDecl for the lambda but got $result")
      }
    }

    "create bindings to implemented method" in {
      cpg.all.collectAll[Binding].nameExact("apply").sortBy(_.signature).toList match {
        case List(erasedBinding, binding) =>
          binding.methodFullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
          binding.signature shouldBe "java.lang.String(java.lang.String)"
          binding.inE.collectAll[Binds].map(_.outNode()).l match {
            case List(typeDecl: TypeDecl) =>
              typeDecl.fullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
            case result => fail(s"Expected typeDecl but got $result")
          }

          erasedBinding.methodFullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
          erasedBinding.signature shouldBe "java.lang.Object(java.lang.Object)"
          erasedBinding.inE.collectAll[Binds].map(_.outNode()).l match {
            case List(typeDecl: TypeDecl) =>
              typeDecl.fullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
            case result => fail(s"Expected typeDecl but got $result")
          }

        case result => fail(s"Expected two bindings to apply method but got $result")
      }
    }
  }

  "lambdas assigned to a variable an a vardecl" should {
    val cpg = code("""
        |import java.util.function.Function;
        |
        |public class Foo {
        |  public void test(String input, String fallback) {
        |    Function<String, String> mapper = lambdaInput -> lambdaInput.length() > 5 ? "Long" : fallback;
        |  }
        |}
        |""".stripMargin)

    // Only test the method node for type info, since this is effectively the same test as above
    // with a different source for the expected type.
    "create a method node for the lambda" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").l match {
        case List(lambdaMethod) =>
          // Lambda body creation tested separately
          lambdaMethod.name shouldBe "lambda$0"
          lambdaMethod.fullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
          lambdaMethod.parameter.l match {
            case List(lambdaInput) =>
              lambdaInput.name shouldBe "lambdaInput"
              lambdaInput.typeFullName shouldBe "java.lang.String"

            case result => fail(s"Expected single lambda parameter but got $result")
          }
          lambdaMethod.methodReturn.typeFullName shouldBe "java.lang.String"

        case result => fail(s"Expected single lambda method but got $result")
      }
    }
  }

  "lambdas reassigned to a variable" should {
    val cpg = code("""
                     |import java.util.function.Function;
                     |
                     |public class Foo {
                     |  public void test(String input, String fallback, Function<String, String> mapper) {
                     |    mapper = lambdaInput -> lambdaInput.length() > 5 ? "Long" : fallback;
                     |  }
                     |}
                     |""".stripMargin)

    // Only test the method node for type info, since this is effectively the same test as above
    // with a different source for the expected type.
    "create a method node for the lambda" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").l match {
        case List(lambdaMethod) =>
          // Lambda body creation tested separately
          lambdaMethod.name shouldBe "lambda$0"
          lambdaMethod.fullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
          lambdaMethod.parameter.l match {
            case List(lambdaInput) =>
              lambdaInput.name shouldBe "lambdaInput"
              lambdaInput.typeFullName shouldBe "java.lang.String"

            case result => fail(s"Expected single lambda parameter but got $result")
          }
          lambdaMethod.methodReturn.typeFullName shouldBe "java.lang.String"

        case result => fail(s"Expected single lambda method but got $result")
      }
    }
  }

  "lambdas returned from a function" should {
    val cpg = code("""
        |import java.util.function.Function;
        |
        |public class Foo {
        |  public Function<String, String> test(String input, String fallback) {
        |    return lambdaInput -> lambdaInput.length() > 5 ? "Long" : fallback;
        |  }
        |}
        |""".stripMargin)

    // Only test the method node for type info, since this is effectively the same test as above
    // with a different source for the expected type.
    "create a method node for the lambda" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").l match {
        case List(lambdaMethod) =>
          // Lambda body creation tested separately
          lambdaMethod.name shouldBe "lambda$0"
          lambdaMethod.fullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
          lambdaMethod.parameter.l match {
            case List(lambdaInput) =>
              lambdaInput.name shouldBe "lambdaInput"
              lambdaInput.typeFullName shouldBe "java.lang.String"

            case result => fail(s"Expected single lambda parameter but got $result")
          }
          lambdaMethod.methodReturn.typeFullName shouldBe "java.lang.String"

        case result => fail(s"Expected single lambda method but got $result")
      }
    }
  }

  "lambdas capturing instance vars" should {
    val cpg = code("""
        |import java.util.function.Consumer;
        |
        |public class Foo {
        |    public String s;
        |
        |    public static void sink(String s) {}
        |
        |    public Consumer<String> test() {
        |        return input -> sink(input + s);
        |    }
        |}
        |""".stripMargin)

    // TODO: Add 0th parameter logic
    "create a 0th `this` parameter" ignore {
      cpg.method.name(".*lambda.*").parameter.l match {
        case List(thisParam, inputParam) =>
          thisParam.name shouldBe "this"
          thisParam.code shouldBe "this"
          thisParam.typeFullName shouldBe "Foo"
          thisParam.order shouldBe 0
          thisParam.index shouldBe 0

          inputParam.name shouldBe "input"
          inputParam.typeFullName shouldBe "java.lang.String"
          inputParam.order shouldBe 1
          inputParam.index shouldBe 1

        case result => fail(s"Expected two params for lambda method but got $result")
      }
    }
  }

  "lambdas calling instance methods" should {
    val cpg = code("""
        |import java.util.function.Consumer;
        |
        |public class Foo {
        |    public static String s;
        |
        |    public void sink(String s) {}
        |
        |    public Consumer<String> test() {
        |        return input -> sink(input + s);
        |    }
        |}
        |""".stripMargin)

    // TODO: Add 0th parameter logic
    "create a 0th `this` parameter" ignore {
      cpg.method.name(".*lambda.*").parameter.l match {
        case List(thisParam, inputParam) =>
          thisParam.name shouldBe "this"
          thisParam.code shouldBe "this"
          thisParam.typeFullName shouldBe "Foo"
          thisParam.order shouldBe 0
          thisParam.index shouldBe 0

          inputParam.name shouldBe "input"
          inputParam.typeFullName shouldBe "java.lang.String"
          inputParam.order shouldBe 1
          inputParam.index shouldBe 1

        case result => fail(s"Expected two params for lambda method but got $result")
      }
    }
  }

  "lambdas using only static context" should {
    val cpg = code("""
        |import java.util.function.Consumer;
        |
        |public class Foo {
        |    public static String s;
        |
        |    public static void sink(String s) {}
        |
        |    public Consumer<String> test() {
        |        return input -> sink(input + s);
        |    }
        |}
        |""".stripMargin)

    "not create a 0th `this` parameter" in {
      cpg.method.name(".*lambda.*").parameter.l match {
        case List(inputParam) =>
          inputParam.name shouldBe "input"
          inputParam.typeFullName shouldBe "java.lang.String"
          inputParam.order shouldBe 1
          inputParam.index shouldBe 1

        case result => fail(s"Expected a single param for lambda method but got $result")
      }
    }
  }

  "lambdas with multiple statements in the body" should {
    val cpg = code("""
                     |import java.util.function.Function;
                     |
                     |public class Foo {
                     |    public static String s;
                     |
                     |    public static String f(String s) { return s;}
                     |
                     |    public Function<String, String> test() {
                     |        return input -> {
                     |          String concatenated = input + s;
                     |          return f(concatenated);
                     |        };
                     |    }
                     |}
                     |""".stripMargin)

    "have the correct method body with locals for captured variables" in {
      cpg.method.name(".*lambda.*").body.astChildren.l match {
        case List(capturedS: Local, concatenated: Local, assignment: Call, ret: Return) =>
          capturedS.order shouldBe 1
          capturedS.name shouldBe "s"
          capturedS.typeFullName shouldBe "java.lang.String"

          concatenated.order shouldBe 2
          concatenated.name shouldBe "concatenated"
          concatenated.typeFullName shouldBe "java.lang.String"

          assignment.order shouldBe 3
          assignment.methodFullName shouldBe Operators.assignment

          ret.order shouldBe 4
          ret.astChildren.l match {
            case List(call: Call) =>
              call.name shouldBe "f"
              call.methodFullName shouldBe "Foo.f:java.lang.String(java.lang.String)"

            case result => fail(s"Expected single return argument but got $result")
          }

        case result => fail(s"Expected 4 children in block but got $result")
      }
    }
  }

  "single-statement lambdas with no return values" should {
    val cpg = code("""
        |import java.util.function.Consumer;
        |
        |public class Foo {
        |    public static void sink(String s) {};
        |
        |    public static Consumer<String> test() {
        |        return input -> sink(input);
        |    }
        |}
        |""".stripMargin)

    "have a method body block without a return statement" in {
      cpg.method.name(".*lambda.*").body.astChildren.l match {
        case List(call: Call) =>
          call.methodFullName shouldBe "Foo.sink:void(java.lang.String)"
          call.argument.l match {
            case List(identifier: Identifier) =>
              identifier.name shouldBe "input"
              identifier.typeFullName shouldBe "java.lang.String"
            case result => fail(s"Expected single String identifier a call arg but got $result")
          }
        case result => fail(s"Expected single call in body but got $result")
      }
    }
  }

  "lambdas implementing a user-defined functional interface" should {
    val cpg = code(
      """
        |public interface Foo<T, R> {
        |
        |    default String foo() {
        |        return "FOO";
        |    }
        |
        |    String baz(T input, R moreInput);
        |
        |    default T bar(T input) {
        |        return input;
        |    }
        |}
        |""".stripMargin,
      fileName = "Foo.java"
    ).moreCode("""
        |public class TestClass {
        |
        |    public static String foo(Integer x, Float y, String z) { return z; }
        |
        |    public static Foo<Integer, Float> test(String captured) {
        |        return (input, moreInput) -> foo(input, moreInput, captured);
        |    }
        |}
        |""".stripMargin)

    "create a method node for the lambda" in {
      cpg.typeDecl.name("TestClass").method.name(".*lambda.*").l match {
        case List(lambdaMethod) =>
          lambdaMethod.name shouldBe "lambda$0"
          lambdaMethod.fullName shouldBe "TestClass.lambda$0:java.lang.String(java.lang.Integer,java.lang.Float)"
          lambdaMethod.signature shouldBe "java.lang.String(java.lang.Integer,java.lang.Float)"
          lambdaMethod.methodReturn.typeFullName shouldBe "java.lang.String"
          lambdaMethod.parameter.l match {
            case List(input, moreInput) =>
              input.name shouldBe "input"
              input.code shouldBe "java.lang.Integer input"
              input.typeFullName shouldBe "java.lang.Integer"
              input.order shouldBe 1
              input.index shouldBe 1

              moreInput.name shouldBe "moreInput"
              moreInput.code shouldBe "java.lang.Float moreInput"
              moreInput.typeFullName shouldBe "java.lang.Float"
              moreInput.order shouldBe 2
              moreInput.index shouldBe 2

            case result => fail(s"Expected two lambda parameters but got $result")
          }

        case result => fail(s"Expected single lambda method but got $result")
      }
    }

    "create the correct binding for the lambda method" in {
      cpg.all.collectAll[Binding].filter(_.name == "lambda$0").l match {
        case List(lambdaBinding) =>
          lambdaBinding.methodFullName shouldBe "TestClass.lambda$0:java.lang.String(java.lang.Integer,java.lang.Float)"
          lambdaBinding.signature shouldBe "java.lang.String(java.lang.Integer,java.lang.Float)"

          lambdaBinding.inE.collectAll[Binds].map(_.outNode()).l match {
            case List(typeDecl: TypeDecl) =>
              typeDecl.fullName shouldBe "TestClass"

            case result => fail(s"Expected single typeDecl but got $result")
          }

        case result => fail(s"Expected single binding for lambda method but got $result")
      }
    }

    "create closure bindings for captured identifiers" in {
      cpg.all.collectAll[ClosureBinding].l match {
        case List(capturedClosureBinding) =>
          capturedClosureBinding.label shouldBe "CLOSURE_BINDING"

          val capturedLocal = cpg.method.name(".*lambda.*").local.name("captured").head
          capturedClosureBinding.closureBindingId shouldBe capturedLocal.closureBindingId

          capturedClosureBinding.outE.collectAll[Ref].map(_.inNode()).l match {
            case List(capturedParam: MethodParameterIn) =>
              capturedParam.name shouldBe "captured"
              capturedParam.method.head.fullName shouldBe "TestClass.test:Foo(java.lang.String)"
            case result => fail(s"Expected single capturedParam but got $result")
          }

          capturedClosureBinding.inE.collectAll[Capture].map(_.outNode()).l match {
            case List(outMethod: MethodRef) =>
              outMethod.methodFullName shouldBe "TestClass.lambda$0:java.lang.String(java.lang.Integer,java.lang.Float)"
            case result => fail(s"Expected single out METHOD_REF but got $result")
          }

        case result => fail(s"Expected 2 closure bindings for captured variables but got $result")
      }
    }

    "create a typeDecl node inheriting from correct interface" in {
      cpg.typeDecl.name(".*lambda.*").l match {
        case List(lambdaDecl) =>
          lambdaDecl.name shouldBe "lambda$0"
          lambdaDecl.fullName shouldBe "TestClass.lambda$0:java.lang.String(java.lang.Integer,java.lang.Float)"
          lambdaDecl.inheritsFromTypeFullName should contain theSameElementsAs List("Foo")

        case result => fail(s"Expected a single typeDecl for the lambda but got $result")
      }
    }

    // TODO: Fix typeDecl for interfaceBinding
    "create bindings to implemented method" ignore {
      cpg.all.collectAll[Binding].nameExact("baz").sortBy(_.methodFullName).toList match {
        case List(interfaceBinding, erasedBinding, binding) =>
          interfaceBinding.methodFullName shouldBe "Foo.baz:java.lang.String(java.lang.Object,java.lang.Object)"
          interfaceBinding.signature shouldBe "java.lang.String(java.lang.Object,java.lang.Object)"
          binding.inE.collectAll[Binds].map(_.outNode()).l match {
            case List(typeDecl: TypeDecl) => typeDecl.fullName shouldBe "Foo"
            case result         => fail(s"Expected typeDecl but got $result")
          }

          binding.methodFullName shouldBe "TestClass.lambda$0:java.lang.String(java.lang.Integer,java.lang.Float)"
          binding.signature shouldBe "java.lang.String(java.lang.Integer,java.lang.Float)"
          binding.inE.collectAll[Binds].map(_.outNode()).l match {
            case List(typeDecl: TypeDecl) =>
              typeDecl.fullName shouldBe "TestClass.lambda$0:java.lang.String(java.lang.Integer,java.lang.Float)"
            case result => fail(s"Expected typeDecl but got $result")
          }

          erasedBinding.methodFullName shouldBe "TestClass.lambda$0:java.lang.String(java.lang.Integer,java.lang.Float)"
          erasedBinding.signature shouldBe "java.lang.Object(java.lang.Object,java.lang.Object)"
          erasedBinding.inE.collectAll[Binds].map(_.outNode()).l match {
            case List(typeDecl: TypeDecl) =>
              typeDecl.fullName shouldBe "TestClass.lambda$0:java.lang.String(java.lang.Integer,java.lang.Float)"
            case result => fail(s"Expected typeDecl but got $result")
          }

        case result => fail(s"Expected three bindings to baz method but got $result")
      }
    }
  }

  // This is an example of a case where all the type info we need to figure out that the lambda implements
  // `Function<String, String>`, but from JavaParser and the current expectedType propagation we only get
  // `java.util.Function<Object, Object>`
  "a lambda implementing a mapper in stream" ignore {
    val cpg = code("""
        |import java.util.List;
        |import java.util.stream.Collectors;
        |
        |public class Foo {
        |  public List<String> method(List<String> list) {
        |    return list.stream().map(string -> string).collect(Collectors.toList());
        |  }
        |}
        |""".stripMargin)

    "create a method node for the lambda" in {
      cpg.typeDecl.name("Foo").method.name(".*lambda.*").l match {
        case List(lambdaMethod) =>
          // Lambda body creation tested separately
          lambdaMethod.name shouldBe "lambda$0"
          lambdaMethod.fullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
          lambdaMethod.parameter.l match {
            case List(lambdaInput) =>
              lambdaInput.name shouldBe "string"
              lambdaInput.typeFullName shouldBe "java.lang.String"

            case result => fail(s"Expected single lambda parameter but got $result")
          }
          lambdaMethod.methodReturn.typeFullName shouldBe "java.lang.String"

        case result => fail(s"Expected single lambda method but got $result")
      }
    }

    "create the correct binding for the lambda method" in {
      cpg.all.collectAll[Binding].filter(_.name == "lambda$0").l match {
        case List(lambdaBinding) =>
          lambdaBinding.methodFullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
          lambdaBinding.signature shouldBe "java.lang.String(java.lang.String)"

          lambdaBinding.inE.collectAll[Binds].map(_.outNode()).l match {
            case List(typeDecl: TypeDecl) =>
              typeDecl.fullName shouldBe "Foo"

            case result => fail(s"Expected single typeDecl but got $result")
          }

        case result => fail(s"Expected single binding for lambda method but got $result")
      }
    }

    "create a typeDecl node inheriting from correct interface" in {
      cpg.typeDecl.name(".*lambda.*").l match {
        case List(lambdaDecl) =>
          lambdaDecl.name shouldBe "lambda$0"
          lambdaDecl.fullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
          lambdaDecl.inheritsFromTypeFullName should contain theSameElementsAs List("java.util.function.Function")

        case result => fail(s"Expected a single typeDecl for the lambda but got $result")
      }
    }

    "create bindings to implemented method" in {
      cpg.all.collectAll[Binding].nameExact("apply").sortBy(_.signature).toList match {
        case List(erasedBinding, binding) =>
          binding.methodFullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
          binding.signature shouldBe "java.lang.String(java.lang.String)"
          binding.inE.collectAll[Binds].map(_.outNode()).l match {
            case List(typeDecl: TypeDecl) =>
              typeDecl.fullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
            case result => fail(s"Expected typeDecl but got $result")
          }

          erasedBinding.methodFullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
          erasedBinding.signature shouldBe "java.lang.Object(java.lang.Object)"
          erasedBinding.inE.collectAll[Binds].map(_.outNode()).l match {
            case List(typeDecl: TypeDecl) =>
              typeDecl.fullName shouldBe "Foo.lambda$0:java.lang.String(java.lang.String)"
            case result => fail(s"Expected typeDecl but got $result")
          }

        case result => fail(s"Expected two bindings to apply method but got $result")
      }
    }
  }
}
