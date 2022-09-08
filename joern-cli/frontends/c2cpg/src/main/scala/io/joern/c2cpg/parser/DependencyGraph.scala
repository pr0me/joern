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

  def getFilename(): String = filename

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

  def canEqual(a: Any) = a.isInstanceOf[Edge]

  override def equals(that: Any): Boolean =
    that match {
        case that: Edge => {
            println("Comparing:")
            println(this)
            println(that)
            println("_____________")
            that.canEqual(this) &&
            this.hashCode == that.hashCode
        }
        case _ => false
    }

  override def hashCode: Int = {
        val prime = 31
        var result = 1
        result = prime * result + start.getFilename().hashCode + end.getFilename().hashCode
        result
  }

  override def toString: String =
    s"${getStart().name} -> ${getEnd().name}"
}

class DependencyGraph(inputPath: String, kDistance: Int) {
  var nodes: Array[Node] = Array()
  var edges: Set[Edge] = Set()

  def compute(): Unit = {
    this.nodes = getNodes()
    this.edges = getEdges(kDistance)

    println("Edges:")
    edges.map(e => println(e))
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

  def getEdges(kDistance: Int): Set[Edge] = {
    // add transitive edges
    var edges: Array[Edge] = Array()
    for (currNode <- this.nodes) {
      var allNextNodes = Array(currNode)
      for (_ <- 0 until kDistance) {
        allNextNodes = allNextNodes.flatMap(v => v.includes.map(include => findNode(include))
          .collect{ case Some(nextNode) => nextNode })

        edges = edges ++ allNextNodes.map(nextNode => new Edge(currNode, nextNode))
      }

    }
    
    edges.toSet
  }

  def findNode(name: String): Option[Node] = {
    this.nodes.find(node => node.name == name)
  }
}
