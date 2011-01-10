/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import eu.tinasoft._

import tinaviz.util.Vector._


object GraphGenerator  {
  implicit def toGraph (g:GraphGenerator) : Graph = g.toGraph
}

class GraphGenerator () {
  
  var nodes = List.empty[Node]
  var properties = Map.empty[String,Any]
  
  println("TODO pass pre-computed metrics here")
  override def nbNodes = nodes.size
  override def nbEdges = { var s = 0;nodes.foreach(s+= _.links.size);s } // had to hack
  
  def addNode(node:Node) = {
    var i = nodes.size
    var inDegree = 0
    nodes.foreach { case m => 
        if (m.hasLink(i)) inDegree += 1
    }
    inDegree
     
    nodes ::= new Node (
      node.uuid,
      node.label,
      node.position,
      node.color,
      node.attributes,
      node.links,
      inDegree,
      node.outDegree
    )
  }

  def setProperty(key:String,value:Any) = {
    properties += key -> value
  }

  /**
   * Used for export to GEXF
   */
  def id (node:Node) : Int = {
    var i = 0
    nodes.foreach {
      case n =>
        if (n==node) return i
        i += 1
    }
    throw new Exception("cannot find id of node "+node)
  }
    
  /**
   * Used for export to GEXF
   */
  def id (uuid:String) : Int = {
    var i = 0
    nodes.foreach{
      case node =>
        if (node.uuid.equals(uuid)) return i
        i += 1
    }
    throw new Exception("cannot find id of node "+uuid)
  }
  
  def node (uuid:String) : Node = {
    nodes.foreach { case node => 
        if (node.uuid.equals(uuid)) return node
    }
    throw new Exception("cannot find node "+uuid)
  }

  def toGraph = {
    //m.nodes.map { case n => Graph.computeNodeDegree(n) }
     Graph.make(nodes, properties)
  }
}
