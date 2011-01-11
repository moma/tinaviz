/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.pipeline

import org.daizoru._

import eu.tinasoft._
import tinaviz.graph._

import tinaviz.util.Vector._
import actors.Actor

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


/**
 * 
 */
class Pipeline(val actor:Actor) extends node.util.Actor {

  start

  var nextState = 'output
  var graph = new Graph()

  def act() {
    while(true) {
      loop {
        react {
          // reset
          case g:Graph =>
            println("we can run the full graph..")
            graph = g
            //cache += 'input -> graph
            //self ! 'colors
            // self


          case 'colors =>
            // only update the color layour, not touching positions!
            //nextState = 'colors
            //cache += 'body -> runColors
            //self ! 'body
            
          case 'body =>
            //nextState = 'body
            //cache += 'layout -> runBody
            //self ! 'layout

          case 'layout =>
            runLayout
            //nextState = 'layout
            //val output = runLayout
            //cache += 'layout -> output
            actor ! 'pipelined -> graph
           
            //case
          case msg => println("unknow msg: "+msg)
        }
      }
    }
  }

  /*
   def runMeso(graph:Graph) = {
   val category = graph.get[String]("filter.category")
   val selection = graph.get[List[String]]("filter.selection")
   println("running meso on "+graph.nbNodes+" nodes")
   val tmp = graph.nodes.filter {
   case n =>
   var connected = false
   graph.nodes.foreach {
   case m =>
   if (m.attributes("category").equals(category))
   if (selection.contains(m.uuid)) connected = true
   }
   (selection.contains(n.uuid) || connected)
   }

   repair(Graph.make(tmp, graph.properties), graph)
   }
   */

  /*
   def runMacro(graph:Graph) = {
   val category = graph.get[String]("filter.category")
   println("running macro on "+graph.nbNodes+" nodes")

   filterBy(graph,"category",category)
   }*/

  /*
  
   def runBody = {
   val graph = cache('input)
   graph.get[String]("filter.view") match {
   case "macro" => runMacro(graph)
   case any => runMeso(graph)
   }
   }
   */
  /*
   def runColors = {
   val graph = cache('input)
   val palette = graph.get[String]("filter.palette")
   println("running meso on "+graph.nbNodes+" nodes")
   val tmp = graph.nodes.map {
   case n =>
        
   }

   repair(Graph.make(tmp, graph.properties), graph)
   }
   */



  /**
   * apply a force vector algorithm on the graph
   */
  def runLayout = {
    val GRAVITY = graph.get[Double]("layout.gravity") // stronger means faster!
    val ATTRACTION = graph.get[Double]("layout.attraction")
    val REPULSION = graph.get[Double]("layout.repulsion")
    println("running forceVector on "+graph.nbNodes+" nodes")
    var i = -1
    val positions = graph.position map {
      case position =>
        i += 1
        var force = position.computeForce(GRAVITY, graph.baryCenter)
        graph linkIdArray i map {
          case j =>
            if (graph.hasAnyLink(i,j)) {
              force += position.computeForce(ATTRACTION, graph.position(j))
            } else {
              force += position.computeForce(REPULSION, graph.position(j))
            }
        }
        position + force
    }

    // TODO possible optimization: give some metrics
    graph = new Graph(graph.elements + ("position" -> (positions.toArray[Any])))
  }


  /*
   def repair(graph:Graph,reference:Graph) = {
   val tmp1 = graph.nodes
   val tmp2 = tmp1.map{
   case node =>
   val links = node.links.map{case (id,weight) => (tmp1.indexOf(reference.node(id)),weight)}
   new Node(node.uuid,
   node.label,
   node.position,
   node.color,
   node.attributes,
   links,
   node.inDegree,
   links.size)
   }
   var i = -1
   val tmp3 = tmp2.map{
   case node =>
   i += 1
   var inDegree = 0
   tmp2.foreach { case m =>
   if (m.hasLink(i)) inDegree += 1
   }
   inDegree
   new Node(node.uuid,
   node.label,
   node.position,
   node.color,
   node.attributes,
   node.links,
   inDegree,
   node.outDegree)
   }
   Graph.make(tmp3, graph.properties)
   }
   */

  /*
   def filterBy(graph:Graph,key:String,value:String) = {
   repair(new Graph(graph.nodes.filter {_.attributes(key).equals(value)},
   graph.properties),
   graph)
   }
   */
}
