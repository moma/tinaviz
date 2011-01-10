/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import MutableNode._

object MutableGraph {
  
  implicit def fromGraph (g:Graph) : MutableGraph = {
    new MutableGraph (
      g.nodes.map { case n => (n:MutableNode) },
      g.properties
    )
  }
  
  implicit def toGraph (m:MutableGraph) : Graph = {
    m.nodes.map { case n => m.computeNodeDegree(n) }
    new Graph(
      m.nodes.map { case n => (n:Node) },
      m.properties,
      m.nbNodes,
      m.nbEdges,
      m.nbSingles,
      m.maxNodeOutDegree,
      m.minNodeOutDegree,
      m.maxNodeInDegree,
      m.minNodeInDegree,
      m.minX,
      m.minY,
      m.maxX,
      m.maxY,
      m.baryCenter
    )
  }
}

class MutableGraph(
  var nodes : List[MutableNode] = List.empty[MutableNode],
  var properties : Map[String,Any] = Map.empty[String,Any]
) {
  println("TODO pass pre-computed metrics here")
  def nbNodes = nodes.size
  def nbEdges = { var s = 0;nodes.foreach(s+= _.links.size);s } // had to hack
  
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

  def nbSingles = {
    var s = 0
    nodes.foreach { 
      case n =>
        if (n.links.size ==0) 
          s += 1
    }
    s 
  }

  def computeNodeDegree (node:MutableNode) = {
    val i = id(node)
    var d = 0
    nodes.foreach { case m => 
        if (m.hasLink(i)) d += 1
    }
    d
    node.inDegree = d
    node
  }
  
  def minNodeOutDegree = {
    var min = if (nbNodes != 0) Int.MaxValue else 0
    nodes.foreach { 
      case n =>
        val s = n.links.size
        if (s < min) min = s
    }
    min
  }
  def maxNodeOutDegree = {
    var max = if (nbNodes != 0) Int.MinValue else 0
    nodes.foreach { 
      case n =>
        val s = n.links.size
        if (s > max) max = s
    }
    max
  }
    

  def maxNodeInDegree = {
    var max =  if (nbNodes != 0) Int.MinValue else 0
    nodes.foreach { 
      case n =>
        val s = n.inDegree
        if (s > max) max = s
    }
    max
  }
    
  def minNodeInDegree = {
    var min = if (nbNodes != 0) Int.MaxValue else 0
    nodes.foreach { 
      case n =>
        val s = n.inDegree
        if (s < min) min = s
    }
    min
  }
  
  
  def minX = {
    var min = if (nbNodes != 0) Double.MaxValue else 0.0
    nodes.foreach { 
      case n =>
        val s = n.position._1
        if (s < min) min = s
    }
    min        
  }
  def minY = {
    var min = if (nbNodes != 0) Double.MaxValue else 0.0
    nodes.foreach { 
      case n =>
        val s = n.position._2
        if (s < min) min = s
    }
    min      
  }
             
  def maxX = {
    var max = if (nbNodes != 0) Double.MinValue else 0.0
    nodes.foreach { 
      case n =>
        val s = n.position._1
        if (s < max) max = s
    }
    max
  }
  
  def maxY = {
    var max = if (nbNodes != 0) Double.MinValue else 0.0
    nodes.foreach { 
      case n =>
        val s = n.position._2
        if (s < max) max = s
    }
    max
  }
  
  def baryCenter = {
    var p = (0.0,0.0)
    nodes.foreach { case n => p = (p._1+n.position._1, p._2+n.position._2)}
    if (nbNodes != 0) (p._1/nbNodes.toDouble,p._2/nbNodes.toDouble) else (0.0,0.0)
  }
  
             
  /**
   * Used for export to GEXF
   */
  def id (node:MutableNode) : Int = {
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
  def id (uuid:String) : Int = {
    var i = 0
    nodes.foreach{
      case node =>
        if (node.uuid.equals(uuid)) return i
        i += 1
    }
    throw new Exception("cannot find id of node "+uuid)
  }
  
  def node (uuid:String) : MutableNode = {
    nodes.foreach { case node => 
        if (node.uuid.equals(uuid)) return node
    }
    throw new Exception("cannot find node "+uuid)
  }

}
