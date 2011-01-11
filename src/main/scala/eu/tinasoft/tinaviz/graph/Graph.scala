/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import eu.tinasoft._
import tinaviz.util.Vector

object Graph {
  
  def make(nodes:Seq[Node],properties:Map[String,Any]) = {
    new Graph (nodes,
               properties,
               nbNodes(nodes),
               nbEdges(nodes),
               nbSingles(nodes),
               outDegree(nodes),
               inDegree(nodes),
               extremums(nodes),
               baryCenter(nodes))
  }
  
  def nbSingles(nodes:Seq[Node]) = {
    var s = 0
    nodes.foreach { 
      case n =>
        if (n.links.size ==0) 
          s += 1
    }
    s 
  }
  
  def nbNodes(nodes:Seq[Node]) = nodes.size
  def nbEdges(nodes:Seq[Node]) = {var s = 0;nodes.foreach(s+= _.links.size);s }


  def computeNodeDegree (nodes:Seq[Node],i:Int) : Int = {
    var d = 0
    nodes.foreach { case m => 
        if (m.hasLink(i)) d += 1
    }
    d
  }
  
  def outDegree(nodes:Seq[Node]) : (Int,Int) = {
    if (nodes.size == 0) return (0,0)
    var max = Int.MinValue 
    var min = Int.MaxValue
    nodes.foreach { 
      case n =>
        val d = n.links.size
        if (d < min) min = d
        if (d < max) max = d
    }
    (min,max)
  }
  def inDegree(nodes:Seq[Node]) : (Int,Int) = {
    if (nodes.size == 0) return (0,0)
    var max = Int.MinValue 
    var min = Int.MaxValue
    var i = 0
    nodes.foreach { 
      case n =>
        var d = 0
        nodes.foreach { case m => 
            if (m.hasLink(i)) d += 1
        }
        d
        if (d < min) min = d
        if (d < max) max = d
        i += 1
    }
    (min,max)
  }
  
  def extremums(nodes:Seq[Node]) = Vector.extremums( nodes.map{case n => n.position } )

  def baryCenter(nodes:Seq[Node]) : (Double,Double) = {
    var p = (0.0,0.0)
    var N = nodes.size.toDouble
    nodes.foreach { case n => p = (p._1+n.position._1, p._2+n.position._2)}
    if (N != 0) (p._1/N,p._2/N) else (0.0,0.0)
  }
  
}

class Graph (val nodes : Seq[Node] = Seq[Node](),
             val properties : Map[String,Any] = Map[String,Any](),
             val nbNodes : Int = 0,
             val nbEdges : Int = 0,
             val nbSingles : Int = 0,
             val outDegree : (Int,Int) = (0,0),
             val inDegree : (Int,Int) = (0,0),
             val extremums : ((Double,Double),(Double,Double)) = ((.0,.0),(.0,.0)),
             val baryCenter : (Double, Double) = (0.0,0.0),
             val revision : Int = 0
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
