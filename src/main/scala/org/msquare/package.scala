

package org

package object msquare {
  type RCD = List[Int]
  import scalax.collection.Graph
  import scalax.collection.edge.LUnDiEdge

  type RCDGraph = Graph[RCD, LUnDiEdge]
  
  type Weight = Int
}