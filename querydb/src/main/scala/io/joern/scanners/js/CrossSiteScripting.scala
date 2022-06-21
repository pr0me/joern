package io.joern.scanners.js

import io.joern.scanners._
import io.joern.console._
import io.joern.macros.QueryMacros._
import io.shiftleft.semanticcpg.language._

object CrossSiteScripting extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def reflectdXSS(): Query =
    Query.make(
      name = "reflected-xss",
      author = Crew.lukas,
      title = "Reflected Cross-Site Scripting: Part of URL returned in Response",
      description = """
            | The Javascript backend (e.g., NodeJS) accesses the .url field of the Request object and  
            | returns parts of it in the HTTP response.
            | Unless the parameter is escaped or validated in-between, this is a reflected XSS vulnerability.
            |""".stripMargin,
      score = 8,
      withStrRep({ cpg =>
        def source = cpg.call.methodFullName(".*fieldAccess.*").where(_.argument(2).isFieldIdentifier.canonicalName(".*url.*"))

        def sink = cpg.call.code(".*\\.write\\(.*\\).*").filter(_.methodFullName != "<operator>.assignment")

        sink.where(_.argument(2).reachableBy(source))
      }),
      codeExamples = CodeExamples(
        List("""
            |function makeResponse(code, message) { 
            |    res.writeHead(code, {"Content-Type" : "text/html"});
            |
            |    res.write(message); 
            |    res.end();
            |}
            |
            |const parsed = route.parse(req.url); 
            |const query = querystring.parse(parsed.query);
            |
            |makeResponse(200, query);
            |""".stripMargin),
        List("""
            |function makeResponse(code, message) { 
            |    res.writeHead(code, {"Content-Type" : "text/html"});
            |
            |    res.write(message); 
            |    res.end();
            |}
            |
            |const parsed = route.parse(req.url); 
            |const query = querystring.parse(parsed.query);
            |doSomething(query);
            |
            |makeResponse(200, "static response");
            |""".stripMargin)
      ),
      tags = List(QueryTags.default, QueryTags.xss)
    )
}