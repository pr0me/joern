package io.joern.c2cpg.utils

import java.io.File
import io.joern.c2cpg.parser.Node
import io.joern.c2cpg.parser.DependencyGraph

// this class implements the SplitMerge algorithm described in the paper 'Input Splitting for Cloud-Based Static Application Security Testing Platforms' by Christakis et al., creating dependency-relative input file splits 
class SplitMergeSplicer(inputPath: String, kDistance: Int = 2, maxSplitSize: Int = 500) {
  val dependencyGraph = new DependencyGraph(inputPath, kDistance)

  def run(): Unit = {
    dependencyGraph.compute()
    val partitions = initPartitions()

    var cappedPartitions: Set[Set[Node]] = Set()
    partitions.map(p => 
      if (p.size > maxSplitSize) cappedPartitions ++= split(p) else cappedPartitions += p)

    println("\ncapped partitions:")
    println(cappedPartitions.map(p => p.map(n => n.name).mkString(", ")).mkString("\n"))
  }

  // create a partition for each node in the dependency graph
  def initPartitions(): Set[Set[Node]] = {
    var partitions: Set[Set[Node]] =  Set()
    dependencyGraph.nodes.map(n => {
      var currPartition: Set[Node] = Set()
      currPartition += n
      n.edges.map(e => currPartition += e.getEnd())
      partitions += currPartition
    })

    partitions
  }

  // split partitions which have more than maxSplitSize elements while preserving high-connectivity 
  def split(partition: Set[Node]): Set[Set[Node]] = {
    var splitArrays: Array[Array[Node]] = Array()
    val sortedByDegreeDesc = partition.toArray.sortBy(n => n.degree).reverse

    // fraction of the high partition
    var p: Int = (partition.size / 3.0).floor.toInt
    if (p > maxSplitSize) 
      p = (maxSplitSize / 3.0).floor.toInt

    val highSplit = sortedByDegreeDesc.slice(0, p)
    val lowSplit = sortedByDegreeDesc.slice(p, sortedByDegreeDesc.size)

    // divide lowSplit uniformly
    val nSubsets = ((partition.size - p) / (maxSplitSize - p).toFloat).floor.toInt + 1
    val nElements = (lowSplit.size / nSubsets.toFloat).ceil.toInt
    splitArrays = lowSplit.sliding(nElements, nElements).toArray

    // include the highSplit, containing nodes of high connectivity, in every split
    splitArrays = splitArrays.map(p => p ++ highSplit)

    var splits: Set[Set[Node]] = Set()
    splitArrays.map(p => splits += p.toSet)
    splits
  }
  
  def merge(partitions: Set[Set[Node]]): Set[Set[Node]] = {
    var mergedPartitions: Set[Set[Node]] = Set()

    partitions.map(p1 => {
      // var isSubset = false
      // if (partitions.forall(p2 => !(p1 != p2 && p1.forall(p2.contains)))) {
      // if (partitions.forall(p2 => !(p1.subsetOf(p2) && p1 != p2))) {
      //   mergedPartitions = mergedPartitions :- p1
      // }

      // partitions.takeWhile(!(_)).map(p2 => {
      //   if (p1.forall(p2.contains) && p1 != p2)
      //     true
      //   else 
      //     false
      // })
    })
    nextFit(mergedPartitions)
  }

  def nextFit(partitions: Set[Set[Node]]): Set[Set[Node]] = {
    var finalPartitions: Set[Set[Node]] = Set()

    finalPartitions
  }

  def populateDirectories(splits: List[List[Node]]): Unit = {
    val splitDirs = splits.map(split => {
      val splitDir = new File("/tmp/" + "split_" + split.hashCode())
      splitDir.mkdir()
      splitDir
    })
    
  }
}
