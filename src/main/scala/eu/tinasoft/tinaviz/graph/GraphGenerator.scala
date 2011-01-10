/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import MutableNode._

object GraphGenerator  {
  
  
  implicit def toGraph (m:MutableGraph) : Graph = {
    //m.nodes.map { case n => Graph.computeNodeDegree(n) }
    new Graph(
      m.nodes.map { case n => (n:Node) },
      m.properties,
      m.nbNodes,
      m.nbEdges,
      m.nbSingles,
      m.outDegree,
      m.inDegree,
      m.extremums,
      m.baryCenter
    )
  }
}

class MutableGraph(
  override var nodes : List[MutableNode] = List.empty[MutableNode],
  override var properties : Map[String,Any] = Map.empty[String,Any]
) extends Graph {
  println("TODO pass pre-computed metrics here")
  override def nbNodes = nodes.size
  override def nbEdges = { var s = 0;nodes.foreach(s+= _.links.size);s } // had to hack
  
  def addNode(node:MutableNode) = {
    /*
     var i = nodes.size
     var d = 0
     nodes.foreach { case m => 
     if (m.hasLink(i)) d += 1
     }
     node.inDegree = d
     */
    nodes ::= node
  }

  def setProperty(key:String,value:Any) = {
    properties += key -> value
  }

  override def nbSingles = Graph.nbSingles(this)
  override def extremums = Graph.extremums(this)
  override def outDegree = Graph.outDegree(this)
  override def inDegree = Graph.inDegree(this)
  override def baryCenter = Graph.baryCenter(this)
  
             
  /**
   * Used for export to GEXF
   */
  override def id (node:MutableNode) : Int = {
    var i = 0
    nodes.foreach{
      case n =>
        if (n==node) return i
        i += 1
    }
    throw new Exception("cannot find id of node "+node)
  }
    
  /**
   * Used for export to GEXF
   */
  override def id (uuid:String) : Int = {
    var i = 0
    nodes.foreach{
      case node =>
        if (node.uuid.equals(uuid)) return i
        i += 1
    }
    throw new Exception("cannot find id of node "+uuid)
  }
  
  override def node (uuid:String) : MutableNode = {
    nodes.foreach { case node => 
        if (node.uuid.equals(uuid)) return node
    }
    throw new Exception("cannot find node "+uuid)
  }

}
