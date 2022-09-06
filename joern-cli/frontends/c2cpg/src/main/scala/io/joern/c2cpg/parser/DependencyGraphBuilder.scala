package io.joern.c2cpg.parser

import io.joern.x2cpg.SourceFiles
import io.joern.c2cpg.datastructures.CGlobal
import io.joern.c2cpg.parser.FileDefaults

class Node(filename: String) {
  object Done extends Exception {}

  val name = filename
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
      .collect { case regex(value) => value }
      .toList

    println(filename)
    println(includeList)

    includeList
  }
}

class Edge(start: Node, end: Node) {
  def getStart(): String = start.name
  def getEnd(): String = end.name
}

class DependencyGraphBuilder(inputPath: String) {
  val nodes = getNodes()
  val edges = getEdges()

  def getFiles(): Array[String] = {
    val sourceFiles = SourceFiles.determine(inputPath, FileDefaults.SOURCE_FILE_EXTENSIONS).toSet
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
    val edges = nodes.flatMap(startNode => startNode.includes.map(include => findNode(nodes, include) match {
        case Some(endNode) => new Edge(startNode, endNode)
      }
    ))
    edges
  }

  def findNode(nodes: Array[Node], filename: String): Option[Node] = {
    nodes.find(node => node.name == filename)
  }
}
