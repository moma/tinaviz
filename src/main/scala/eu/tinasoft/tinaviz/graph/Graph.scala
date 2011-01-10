/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

object Graph {
  def nbSingles(g:Graph) = {
    var s = 0
    g.nodes.foreach { 
      case n =>
        if (n.links.size ==0) 
          s += 1
    }
    s 
  }

  def computeNodeDegree (g:Graph,i:Int) : Int = {
    var d = 0
    g.nodes.foreach { case m => 
        if (m.hasLink(i)) d += 1
    }
    d
  }
  
  def outDegree(g:Graph) = {
    var max = if (g.nbNodes != 0) Int.MinValue else 0
    var min = if (g.nbNodes != 0) Int.MaxValue else 0
    g.nodes.foreach { 
      case n =>
        val d = n.links.size
        if (d < min) min = d
        if (d < max) max = d
        
    }
    (min,max)
  }
  def inDegree(g:Graph) = {
    var max = if (g.nbNodes != 0) Int.MinValue else 0
    var min = if (g.nbNodes != 0) Int.MaxValue else 0
    g.nodes.foreach { 
      case n =>
        val d = Graph.computeNodeDegree(g, g.id(n))
        if (d < min) min = d
        if (d < max) max = d
    }
    (min,max)
  }
  
  /**
   * Return the extremums for X (min,max) and Y (min,max)
   */
  def extremums(g:Graph) : ((Double,Double),(Double,Double)) = {
    var minX = if (g.nbNodes != 0) Double.MaxValue else 0.0
    var minY = if (g.nbNodes != 0) Double.MaxValue else 0.0
    var maxX = if (g.nbNodes != 0) Double.MinValue else 0.0
    var maxY = if (g.nbNodes != 0) Double.MinValue else 0.0
    g.nodes.foreach { 
      case n =>
        val x = n.position._1
        val y = n.position._2
        if (x < minX) minX = x
        if (x > maxX) maxX = x
        if (y < minY) minX = y
        if (y > maxY) maxX = y
    }
    ((minX,maxX),(minY,maxY))  
  }

  def baryCenter(g:Graph) : (Double,Double) = {
    var p = (0.0,0.0)
    g.nodes.foreach { case n => p = (p._1+n.position._1, p._2+n.position._2)}
    if (g.nbNodes != 0) (p._1/g.nbNodes.toDouble,p._2/g.nbNodes.toDouble) else (0.0,0.0)
  }
  
}

class Graph (val nodes : List[Node] = List[Node](),
             val properties : Map[String,Any] = Map[String,Any](),
             val nbNodes : Int = 0,
             val nbEdges : Int = 0,
             val nbSingles : Int = 0,
             val outDegree : (Int,Int) = (0,0),
             val inDegree : (Int,Int) = (0,0),
             val extremums : ((Double,Double),(Double,Double)) = ((.0,.0),(.0,.0)),
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
