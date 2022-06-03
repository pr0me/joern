package io.joern.scanners.jssrc

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
            |    // XSS vulnerability
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
            |    // XSS vulnerability
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

}