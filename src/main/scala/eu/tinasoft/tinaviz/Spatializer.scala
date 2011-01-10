/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import org.daizoru._

import eu.tinasoft._
import tinaviz.graph._
import processing.core.PApplet
import tinaviz.graph.MutableGraph._
import tinaviz.graph.MutableNode._
import tinaviz.util.Vector._


object NodeForce {   
  implicit def fromMutableNode (n:MutableNode) = new NodeForce(n)
  implicit def toMutableNode (nf:NodeForce) = nf.node
}

class NodeForce (val node:MutableNode) {
  def applyForce(source:(Double,Double),f:Double) = {
    val dx = source._1 - node.position._1
    val dy = source._2 - node.position._2
    val d = math.sqrt(dx*dx + dy*dy)
    if (d!=0.0) node.velocity +=  ((dx / d) * f, (dy / d) * f)
    node
  }
}

import NodeForce._

object Spatializer {
    

}

class Spatializer extends node.util.Actor {

  start
  
  def act() {

    while(true) {
      loop {
        react {
          
          case graph:Graph =>
            println("running layout algorithm..")
            val g = forceVector(graph)
            println("layout finished step")
           

            //case
          case msg => println("unknow msg: "+msg)
        }
      }
    }

    
  }
  
  val GRAVITY = 1.0
  /**
   * apply a force vector algorithm on the graph
   */
  def forceVector(graph:Graph) : Graph = {
    val g = graph:MutableGraph
    var nid = 0
    g.nodes = graph.nodes.map {
      case node => 
        val n = node:MutableNode // node A (current)
        // node.links.foreach {
        //case (mid,weight) =>
        // val m = graph.node(mid) // node B (neighbour)
        //  if (nid != mid) {
            
        //  m.force = (n.force._1,n.force._2)
        //  }
        //  m
        //  
        n.applyForce(graph.baryCenter, GRAVITY)
    }
    g:Graph
  }

}
