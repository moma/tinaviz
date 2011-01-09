/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

object MutableGraph {

  implicit def mutableGraphToGraph (m:MutableGraph) : Graph = {
    m.toGraph
  }
}
class MutableGraph(
  var nodes : List[MutableNode] = List.empty[MutableNode],
  var properties : Map[String,Any] = Map.empty[String,Any]
) {
  
  def nbNodes = nodes.size
  def nbEdges = { var s = 0;nodes.foreach(s+= _.links.size);s } // had to hack
  
  def addNode(node:MutableNode) = {
   nodes ::= node
  }

  def setProperty(key:String,value:Any) = {
    properties += key -> value
  }

  
  /**
   * Used for export to GEXF
   */
  def id (node:Node) : Int = {
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
  /**
   * Return a new immutable graph
   */
  def toGraph = new Graph(nodes.map(_.toNode), properties)
}
