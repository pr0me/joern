package io.joern.scanners.js

import io.joern.scanners._
import io.joern.console._
import io.joern.macros.QueryMacros._
import io.shiftleft.semanticcpg.language._

object TestabilityTarpit extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def restParamUsed(): Query =
    Query.make(
      name = "rest-parameter",
      author = Crew.lukas,
      title = "Tarpit: Variadic `rest` parameter in use",
      description = """
            | The rest parameter syntax (three dots) allows to represent an indefinite number of arguments as an array. 
            | This makes it more complicated for a static analyzer to reason about the data flow.
            |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        cpg.method.filterNot(_.isExternal).code(".*\\.\\.\\..*")
      }),
      codeExamples = CodeExamples(
        List("""
            |
            |function sum(...numbers){
            |    res.writeHead(200, {"Content-Type" : "text/html"});
            |    numbers.forEach(out);
            |    res.end();
            |}
            |
            |function out(val){
            |    // XSS vulnerability not found by 4/5 scanners
            |    res.write(val); 
            |}
            |
            |const parsed = route.parse(req.url);
            |const query  = querystring.parse(parsed.query);
            |var b = query.name;
            |sum('a', 'b', 'c', b);
            |""".stripMargin),
        List("""
            |// could be refactored to:
            |function sum(v1, v2, v3, v4, v5){
            |    res.writeHead(200, {"Content-Type" : "text/html"});
            |    out(v1);
            |    out(v2);
            |    out(v3);
            |    out(v4);
            |    out(v5);
            |    res.end();
            |}
            |
            |function out(val){
            |    // XSS vulnerability can now be found
            |    res.write(val); 
            |}
            |
            |const parsed = route.parse(req.url);
            |const query  = querystring.parse(parsed.query);
            |var b = query.name;
            |sum('a', 'b', 'c', b);
            |""".stripMargin)
      ),
      tags = List(QueryTags.testabilityTarpit, QueryTags.xss)
    )

  @q
  def functionAsArg(): Query =
    Query.make(
      name = "callback-function",
      author = Crew.lukas,
      title = "Tarpit: Passing a function as an argument",
      description = """
            | The function is assigned to a variable and passed as a parameter to another function. 
            | Calling a function "dynamically" may lead to instances in which static analyzers do not find XSS vulnerabilities.
            |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        var n = cpg.method.name.l
        cpg.method.filterNot(_.isExternal).callIn.argument.isIdentifier.filter(x => n.contains(x.name))
      }),
      codeExamples = CodeExamples(
        List("""
            |
            |function MyFunction(n) {
            |    return n;
            |}
            | 
            |function print(n, message) {
            |    res.writeHead(200, {"Content-Type" : "text/html"});
            |
            |    // XSS vulnerability not found by 4/5 scanners
            |    res.write(message(n)); 
            |    res.end();
            |}
            |
            |const parsed = route.parse(req.url);
            |const query  = querystring.parse(parsed.query);
            |var n = query.name;
            |print(n, MyFunction);
            |""".stripMargin),
        List("""
            |// could be refactored to:
            |
            |function MyFunction(n) {
            |    return n;
            |}
            | 
            |function print(n) {
            |    res.writeHead(200, {"Content-Type" : "text/html"});
            |    res.write(MyFunction(n)); 
            |    res.end();
            |}
            |
            |const parsed = route.parse(req.url);
            |const query  = querystring.parse(parsed.query);
            |const n = query.name;
            |print(n);
            |""".stripMargin)
      ),
      tags = List(QueryTags.testabilityTarpit, QueryTags.xss)
    )

  @q
  def objectCreateUsed(): Query =
    Query.make(
      name = "object-create",
      author = Crew.lukas,
      title = "Tarpit: Using a local existing object as prototype of newly created object",
      description = """
            | Object.create permits to create an object, using an existing object as the prototype of the newly created object.
            | If the original object carries user-supplied data, this may lead to undetected XSS vulnerabilities.
            |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        var assignments = cpg.method(".*assignment.*").callIn.argument.isIdentifier.name.toSet

        cpg.method
          .filter(_.fullName == "Object.create")
          .callIn
          .argument
          .isIdentifier
          .filter(x => assignments.contains(x.name))
          .astParent
      }),
      codeExamples = CodeExamples(
        List("""
            |
            |const parsed = route.parse(req.url); 
            |const query = querystring.parse(parsed.query); 
            |let b = query.name; 
            |
            |const obj = {
            |    name:b
            |};
            |          
            |const obj2 = Object.create(obj); 
            |
            |res.writeHead(200, {"Content-Type" : "text/html"});
            |
            |// XSS undetected by 3/5 scanners
            |res.write(obj2.name);
            |res.end();
            |""".stripMargin),
        List("""
            |""".stripMargin)
      ),
      tags = List(QueryTags.testabilityTarpit, QueryTags.xss)
    )

  @q
  def objectAssignmentUsed(): Query =
    Query.make(
      name = "object-assign",
      author = Crew.lukas,
      title = "Tarpit: Assignement between two objects",
      description = """
            | When assigning between two objects, changes on one affect the other.
            | This may lead to aggravated analyzability for static vulnerability scanners.
            |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        var new_objects = cpg.method(".*assignment.*").callIn.code(".*= new.*").argument.isIdentifier.name.l

        cpg
          .method(".*assignment.*")
          .callIn
          .argument
          .isIdentifier
          .filter(x => new_objects.contains(x.name))
          .filter(x => x.order == 2)
          .astParent
      }),
      codeExamples = CodeExamples(
        List("""
            |class myClass { 
            |    b = 'safe';
            |}
            |const parsed = route.parse(req.url); 
            |const query = querystring.parse(parsed.query); 
            |obj1 = new myClass(); 
            |obj2 = obj1; 
            |obj2.b = query.name;
            |// XSS
            |res.writeHead(200, {"Content-Type" : "text/html"}); 
            |res.write(obj1.b);
            |res.end();
            |""".stripMargin),
        List("""
            |""".stripMargin)
      ),
      tags = List(QueryTags.testabilityTarpit, QueryTags.xss)
    )

  @q
  def bindMethodUsed(): Query =
    Query.make(
      name = "bind-method",
      author = Crew.lukas,
      title = "Tarpit: Function creation through bind() method",
      description = """
            | The bind() method creates a new function that, when called, has its `this` keyword set to the provided value.
            | This can lead to obfuscated data flow, which may lead to undetected XSS vulnerabilities.
            |""".stripMargin,
      score = 2,
      withStrRep({ cpg =>
        cpg.call.code(".*\\.bind\\(.*\\)")
      }),
      codeExamples = CodeExamples(
        List("""
            |function getX(x){
            |    return x;
            |}
            |
            |const parsed = route.parse(req.url);
            |const query  = querystring.parse(parsed.query);
            |let b = query.name;
            |
            |const boundGetX = getX.bind();
            |
            |res.writeHead(200, {"Content-Type" : "text/html"});
            |//XSS, undetected by 4/5 scanners
            |res.write(boundGetX(b));
            |res.end();
            |""".stripMargin),
        List("""
            |""".stripMargin)
      ),
      tags = List(QueryTags.testabilityTarpit, QueryTags.xss)
    )

}
