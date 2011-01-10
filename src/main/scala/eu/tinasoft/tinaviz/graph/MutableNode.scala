/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import eu.tinasoft._

import tinaviz.util.Color
import tinaviz.util.Color._

object MutableNode {
    implicit def toNode (mn:MutableNode) : Node = {
    new Node ( mn.uuid,
              mn.label,
              mn.position,
              mn.velocity,
              mn.color,
              //size,
              mn.attributes,
              mn.links,
              mn.inDegree,
              mn.links.size) 
  }
  implicit def fromNode (n:Node) : MutableNode = {
    new MutableNode ( n.uuid,
              n.label,
              n.position,
              n.velocity,
              n.color,
              //size,
              n.attributes,
              n.links,
              n.inDegree) 
  }
}

case class MutableNode (
  var uuid : String,
  var label : String = "Node",
  var position : (Double,Double) = (0,0),
  var velocity : (Double,Double) = (0,0),
  var color : Color = new Color(0,0,0),
  //var size : (Double) = 1,
  //var category : String = "NGram",
  var attributes : Map[String,Any] =  Map.empty[String,Any],
  var links : List[(Int,Double)] = List.empty[(Int,Double)],
  var inDegree : Int = 0
) {

  override val toString = "<Node "+uuid+":"+label+":"+position._1+","+position._2+">"

  
  def addNeighbour(id:Int,weight:Double) = links ::= (id,weight)
  
  def hasLink(id:Int) : Boolean = { 
    links.foreach { 
      case (i,w) => 
        if (i==id) return true 
    }
    return false
  }
  
}
