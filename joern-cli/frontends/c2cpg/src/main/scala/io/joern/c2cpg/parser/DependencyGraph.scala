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
  def getStart(): Node = start
  def getEnd(): Node = end

  override def toString: String =
    s"${getStart().name} -> ${getEnd().name}"
}

class DependencyGraph(inputPath: String, kDistance: Int) {
  var nodes: Array[Node] = Array()
  var edges: Array[Edge] = Array()

  def compute(): Unit = {
    this.nodes = getNodes()
    this.edges = getEdges()

    if (kDistance > 1) {
      addTransitiveEdges()
    }
  }

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
      .flatMap(startNode => startNode.includes.map(include => findNode(nodes, include))
      .collect{ case Some(endNode) => new Edge(startNode, endNode) })

    edges.map(e => println(e))
    edges
  }

  def addTransitiveEdges(): Unit = {
    // val transitiveEdges = this.edges
    //   .flatMap(edge => edge.getEnd().includes.map(include => findNode(this.nodes, include))
    //   .collect{ case Some(endNode) => new Edge(edge.getEnd(), endNode) })

    // transitiveEdges.map(e => println(e))
  }

  def findNode(nodes: Array[Node], name: String): Option[Node] = {
    nodes.find(node => node.name == name)
  }
}
