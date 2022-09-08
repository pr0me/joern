package io.joern.c2cpg.parser

import io.joern.x2cpg.SourceFiles
import io.joern.c2cpg.datastructures.CGlobal
import io.joern.c2cpg.parser.FileDefaults

class Node(filename: String) {
  object Done extends Exception {}

  val name = filename.split("/").last
  val includes = getIncludes(filename)
  val filetype = if (filename.endsWith(".c") || filename.endsWith(".cpp"))
        "source"
      else if (filename.endsWith(".h") || filename.endsWith(".hpp"))
        "header"

  def getIncludes(filename: String): List[String] = {
    val currFile: java.io.File = new java.io.File(filename)
    val regex = """#include\s*["<](.*)[">]""".r 
    val includeList = scala.io.Source.fromFile(currFile)
      .getLines()
      .collect{ case regex(value) => value }
      .toList

    println(includeList)

    includeList
  }
}

class Edge(start: Node, end: Node) {
  def getStart(): String = start.name
  def getEnd(): String = end.name
  def print(): Unit = println(s"${getStart()} -> ${getEnd()}")
}

class DependencyGraphBuilder(inputPath: String) {
  val nodes = getNodes()
  val edges = getEdges()

  def getFiles(): Array[String] = {
    val sourceFiles            = SourceFiles.determine(inputPath, FileDefaults.SOURCE_FILE_EXTENSIONS).toSet
    val allHeaderFiles         = SourceFiles.determine(inputPath, FileDefaults.HEADER_FILE_EXTENSIONS).toSet
    val alreadySeenHeaderFiles = CGlobal.headerFiles
    val headerFiles = allHeaderFiles -- alreadySeenHeaderFiles
    
    val allFiles = sourceFiles ++ headerFiles

    allFiles.toArray
  }

  def getNodes(): Array[Node] = {
    val files = getFiles()
    val nodes = files.map(file => new Node(file))
    nodes
  }

  def getEdges(): Array[Edge] = {
    val edges = this.nodes
      .flatMap(startNode => startNode.includes.map(include => findNode(include))
      .collect{ case Some(endNode) => new Edge(startNode, endNode) })

    edges.map(e => e.print())
    edges
  }

  def findNode(name: String): Option[Node] = {
    this.nodes.find(node => node.name == name)
  }
}
