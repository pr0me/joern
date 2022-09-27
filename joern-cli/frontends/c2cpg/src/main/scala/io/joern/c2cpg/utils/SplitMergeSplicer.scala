package io.joern.c2cpg.utils

import java.io.File
import io.joern.c2cpg.parser.Node
import io.joern.c2cpg.parser.DependencyGraph

// this class implements the SplitMerge algorithm described in the paper 'Input Splitting for Cloud-Based Static Application Security Testing Platforms' by Christakis et al., creating dependency-relative input file splits 
class SplitMergeSplicer(inputPath: String, kDistance: Int = 2, maxSplitSize: Int = 0) {
  val dependencyGraph = new DependencyGraph(inputPath, kDistance)

  def run(): Unit = {
    dependencyGraph.compute()
    val partitions = initPartitions()
    val reducedPartitions = split(partitions)
  }

  // creates a partition for each node in the dependency graph
  def initPartitions(): Array[Array[Node]] = {
    var partitions: Array[Array[Node]] =  Array()
    dependencyGraph.nodes.map(n => {
      var currPartition: Array[Node] = Array()
      currPartition = currPartition :+ n
      n.edges.map(e => currPartition = currPartition :+ e.getEnd())
      partitions = partitions :+ currPartition
    })

    println("\nPartitions:")
    println(partitions.map(p => p.map(n => n.name).mkString(",")).mkString("\n"))
    println("\n")
    partitions
  }

  // splits partitions which have more than maxSplitSize elements while preserving high-connectivity 
  def split(partitions: Array[Array[Node]]): Array[Array[Node]] = {
    val splits: Array[Array[Node]] =  Array()
    
    
    splits
  }
  
  def merge(): Unit = {

  }

  def populateDirectories(splits: List[List[Node]]): Unit = {
    val splitDirs = splits.map(split => {
      val splitDir = new File("/tmp/" + "split_" + split.hashCode())
      splitDir.mkdir()
      splitDir
    })
    
  }
}
