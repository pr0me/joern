package io.joern.c2cpg.utils

import java.io.File
import io.joern.c2cpg.parser.Node
import io.joern.c2cpg.parser.DependencyGraphBuilder

// this class implements the SplitMerge algorithm described in the paper 'Input Splitting for Cloud-Based Static Application Security Testing Platforms' by Christakis et al., creating dependency-relative input file splits 
class SplitMergeSplicer(inputPath: String, kDistance: Int = 2, maxSplitSize: Int = 0) {
  
  def run(): Unit = {
    val dependencyGraph = new DependencyGraphBuilder(inputPath, kDistance)
  }

  def initPartitions(): Unit = {

  }

  def split(): Unit = {

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
