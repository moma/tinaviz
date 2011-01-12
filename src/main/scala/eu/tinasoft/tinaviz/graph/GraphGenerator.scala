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

class GraphGenerator (
  var nodes : List[Node] = List.empty[Node],
  var properties : Map[String,Any] = Map.empty[String,Any]
) {
  

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
    var elements = Map.empty[String,List[Any]]
    def add(kv:(String,Any)) = {
      if (!elements.contains(kv._1))
        elements(kv._1) = List(kv._2)
      else
        elements(kv._1) ::= kv._2
    }
    nodes.foreach{
      case n => 
        add("uuid" -> n.uuid)
        add("label" -> n.label)
        add("position" -> n.position)
        add("color" -> n.color)
        add("size" -> 1.0)
        add("linkIdArray" -> n.links.map(_._1).toArray)
        add("linkWeightArray"->  n.links.map(_._2).toArray)
        add("linkSet" -> n.links.map(_._1).toSet)
        n.attributes.foreach{case kv => add(kv)}
    }
    Graph.make(elements.map{case (key,values) => (key,values.toArray) })
  }
}
