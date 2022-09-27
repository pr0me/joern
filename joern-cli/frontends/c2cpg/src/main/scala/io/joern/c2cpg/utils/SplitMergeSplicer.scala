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

    var cappedPartitions: Array[Array[Node]] = Array()
    partitions.map(p => 
      cappedPartitions = if (p.size > maxSplitSize) cappedPartitions ++ split(p) else cappedPartitions :+ p)

    println("\ncapped partitions:")
    println(cappedPartitions.map(p => p.map(n => n.name).mkString(", ")).mkString("\n"))
  }

  // create a partition for each node in the dependency graph
  def initPartitions(): Array[Array[Node]] = {
    var partitions: Array[Array[Node]] =  Array()
    dependencyGraph.nodes.map(n => {
      var currPartition: Array[Node] = Array()
      currPartition = currPartition :+ n
      n.edges.map(e => currPartition = currPartition :+ e.getEnd())
      partitions = partitions :+ currPartition
    })

    partitions
  }

  // split partitions which have more than maxSplitSize elements while preserving high-connectivity 
  def split(partition: Array[Node]): Array[Array[Node]] = {
    var splits: Array[Array[Node]] =  Array()
    val sortedByDegreeDesc = partition.sortBy(n => n.degree).reverse

    // fraction of the high partition
    var p: Int = (partition.size / 3.0).floor.toInt
    if (p > maxSplitSize) 
      p = (maxSplitSize / 3.0).floor.toInt

    val highSplit = sortedByDegreeDesc.slice(0, p)
    val lowSplit = sortedByDegreeDesc.slice(p, sortedByDegreeDesc.size)

    // divide lowSplit uniformly
    val nSubsets = ((partition.size - p) / (maxSplitSize - p).toFloat).floor.toInt + 1
    val nElements = (lowSplit.size / nSubsets.toFloat).ceil.toInt
    splits = lowSplit.sliding(nElements, nElements).toArray

    // include the highSplit, containing nodes of high connectivity, in every split
    splits = splits.map(p => p ++ highSplit) 
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
