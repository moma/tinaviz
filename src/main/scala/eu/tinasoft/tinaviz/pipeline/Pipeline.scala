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
 * 
 */
class Pipeline(val actor:Actor) extends node.util.Actor {

  start

  var nextState = 'output
  var data = new Graph()

  def act() {
    while(true) {
      loop {
        react {
          // reset
          case g:Graph =>
            println("we can run the full graph..")
            data = g
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
            println("running layout")
            runLayout
            //nextState = 'layout
            //val output = runLayout
            //cache += 'layout -> output
            println("sending data back to the actor")
            actor ! 'pipelined -> data
           
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
    val GRAVITY = data.get[Double]("layout.gravity") // stronger means faster!
    val ATTRACTION = data.get[Double]("layout.attraction")
    val REPULSION = data.get[Double]("layout.repulsion")
    val barycenter = data.get[(Double,Double)]("baryCenter")
    val nbNodes = data.get[Int]("nbNodes")
    println("running forceVector on "+nbNodes+" nodes")
    var i = -1
    val positions = data.position map {
      case position =>
        i += 1
        var force = position.computeForce(GRAVITY, barycenter)
        data linkIdArray i map {
          case j =>
            if (data.hasAnyLink(i,j)) {
              force += position.computeForce(ATTRACTION, data position j)
            } else {
              force += position.computeForce(REPULSION, data position j)
            }
        }
        position + force
    }

    // TODO possible optimization: give some metrics
    data +="position" -> positions
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
