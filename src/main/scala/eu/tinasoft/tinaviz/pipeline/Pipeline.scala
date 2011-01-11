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


class Pipeline(val actor:Actor) extends node.util.Actor {

  start

  var nextState = 'output
  var cache = Map('input -> new Graph(),
                  'body -> new Graph(),
                  'layout -> new Graph())
  def act() {
    while(true) {
      loop {
        react {
          // reset
          case graph:Graph =>
            println("we can run the full graph..")
            cache += 'input -> graph
            self ! 'body
          
          case 'body =>
            nextState = 'body
            cache += 'layout -> runBody
            self ! 'layout

          case 'layout =>
            nextState = 'layout
            val output = runLayout
            cache += 'layout -> output
            actor ! 'pipelined -> output
           
            //case
          case msg => println("unknow msg: "+msg)
        }
      }
    }
  }

  def runMeso(graph:Graph) = {
    val category = graph.get[String]("filter.category")
    val selection = graph.get[List[String]]("filter.selection")
    println("running meso on "+graph.nbNodes+" nodes")
    var i = -1
    val tmp = graph.nodes.filter { 
      case n => 
        i += 1
        var connected = false
        graph.nodes.foreach {
          case m =>
            if (m.attributes("category").equals(category)) 
              if (selection.contains(m.uuid)) connected = true
        }
        (selection.contains(n.uuid) || connected)
    }
    val newGraph = Graph.make(tmp, graph.properties)
    repair(newGraph, graph)
  }

  
  def runMacro(graph:Graph) = {
    val category = graph.get[String]("filter.category")
    println("running macro on "+graph.nbNodes+" nodes")

    repair(filterBy(graph,"category",category), graph)
  }
  
  def runBody = {
    val graph = cache('input)
    graph.get[String]("filter.view") match {
      case "macro" => runMacro(graph)
      case any => runMeso(graph)
    }
  }
  

  /**
   * apply a force vector algorithm on the graph
   */
  def runLayout = {
  
    val graph = cache('layout)

                      
    val GRAVITY = graph.get[Double]("layout.gravity") // stronger means faster!
    val ATTRACTION = graph.get[Double]("layout.attraction")
    val REPULSION = graph.get[Double]("layout.repulsion")


    println("running forceVector on "+graph.nbNodes+" nodes")
    
    var id = -1
    val nodes = graph.nodes.map {
      case node => 
        id += 1
        //val n = node:MutableNode // node A (current)
        var force = node.computeForce(graph.baryCenter, GRAVITY)
        
        // for all other nodes
        graph.nodes.foreach { 
          case pair =>

            //(1.2, 4.5) + (4.0, 1.0)
            // link between them
            if (pair.hasLink(id) | node.hasLink(id)) {
              force += node.computeForce(pair.position, ATTRACTION)
            } else {
              force += node.computeForce(pair.position, REPULSION)
            }
             
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
  
  def filterBy(graph:Graph,key:String,value:String) = {
    repair(new Graph(graph.nodes.filter {_.attributes(key).equals(value)},
                     graph.properties), 
           graph)
  }
}
