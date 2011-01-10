/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

class Graph (val nodes : List[Node] = List[Node](),
             val properties : Map[String,Any] = Map[String,Any](),
             val nbNodes : Int = 0,
             val nbEdges : Int = 0,
             val nbSingles : Int = 0,
             val maxNodeOutDegree : Int = 0,
             val minNodeOutDegree : Int = 0,
             val maxNodeInDegree : Int = 0,
             val minNodeInDegree : Int = 0,
             val minX : Double = 0.0,
             val minY : Double = 0.0,
             val maxX :Double = 0.0,
             val maxY : Double = 0.0,
             val baryCenter : (Double, Double) = (0.0,0.0)
) {
  
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
    nodes.foreach { 
      case node => 
        if (node.uuid.equals(uuid)) return node
    }
    throw new Exception("cannot find node "+uuid)
  }
  
  def node (id:Int) : Node = {
    if (id < 0 )  throw new Exception("error, cannot found node id "+id)
    if (id > nodes.size )  throw new Exception("error, cannot found node id "+id)
    return nodes(id)
  }

}
