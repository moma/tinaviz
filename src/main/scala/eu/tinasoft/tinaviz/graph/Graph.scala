/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

class Graph (val nodes : List[Node] = List[Node](),
             val properties : Map[String,Any] = Map[String,Any]()) {
  
  val nbNodes = nodes.size
  val nbEdges = { var s = 0;nodes.foreach(s+= _.links.size);s } // had to hack

  
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
  
  
  def node (uuid:String) : Node = {
    nodes.foreach { case node => 
        if (node.uuid.equals(uuid)) return node
    }
    throw new Exception("cannot find node "+uuid)
  }
  
  def node (id:Int) : Node = {
    if (id < 0 )  throw new Exception("error, cannot found node id "+id)
    if (id > nodes.size )  throw new Exception("error, cannot found node id "+id)
    return nodes(id)
  }
  

  def toGraph = new Graph(nodes, properties)
}
