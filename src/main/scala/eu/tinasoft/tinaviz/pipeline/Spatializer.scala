/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.pipeline

import org.daizoru._

import eu.tinasoft._
import tinaviz.graph._

import tinaviz.util.Vector._
  
/**
 * A Node Wrapper, to directly apply forces
 */
object NodeForce {   
  implicit def fromNode (n:Node) = new NodeForce(n)
  implicit def toNode (nf:NodeForce) = nf.node
}
class NodeForce (val node:Node) {

  
  /*
   def applyForce(source:(Double,Double),f:Double) = {
   val dx = source._1 - node.position._1
   val dy = source._2 - node.position._2
   val d = math.sqrt(dx*dx + dy*dy)
   if (d!=0.0) node.velocity +=  ((dx / d) * f, (dy / d) * f)
   node
   }
   */
  def computeForce(source:(Double,Double),f:Double) : (Double,Double) = {
    val dx = source._1 - node.position._1
    val dy = source._2 - node.position._2
    val d = math.sqrt(dx*dx + dy*dy)
    //println("  d: "+d)
    if (d!=0.0) ((dx / d) * f, (dy / d) * f) else (0.0,0.0)
  }

  /*
   def applyVelocity(vel:(Double,Double)) = {
   node.velocity = vel
   }
   */
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
            //println("running layout algorithm on "+graph.nbNodes+" ("+graph.nodes.size+") nodes..")
            val g = forceVector(graph)
            //println("layout finished step")
            reply(('spatialized,g))
           

            //case
          case msg => println("unknow msg: "+msg)
        }
      }
    }

    
  }
  
  val GRAVITY = 1.2 // stronger means faster!
  val ATTRACTION = 100
  val REPULSION = - 1.4
  
  /**
   * apply a force vector algorithm on the graph
   */
  def forceVector(graph:Graph) : Graph = {
    //val g = graph:MutableGraph

    println("running forceVector on "+graph.nbNodes+" nodes")
    
    var id = -1
    val nodes = graph.nodes.map {
      case node => 
        id += 1
        //val n = node:MutableNode // node A (current)
        var force = (0.0,0.0)
        force += node.computeForce(graph.baryCenter, GRAVITY)
        
        // for all other nodes
        graph.nodes.foreach { 
          case pair =>
            
            // link between them
            //if (pair.hasLink(id) | node.hasLink(id)) {
              force += node.computeForce(pair.position, ATTRACTION)
           // } else {
              //force += node.computeForce(pair.position, REPULSION)
           // }
             
             
        }
        // val m = graph.node(mid) // node B (neighbour)
        //  if (nid != mid) {
            
        //  m.force = (n.force._1,n.force._2)
        //  }
        //  m
        //  
        // println("  - pos: "+n.position+" v: "+v)
        new Node(node.uuid,
             node.label,
             force + node.position,
             node.color,
             node.attributes,
             node.links,
             node.inDegree,
             node.outDegree)
    }
    
     Graph.make(nodes, graph.properties)
  }

}
