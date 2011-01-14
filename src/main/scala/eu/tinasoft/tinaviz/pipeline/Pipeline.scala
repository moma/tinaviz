/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.pipeline

import org.daizoru._

import eu.tinasoft._
import tinaviz.graph._
import tinaviz.sketch.Sketch
import tinaviz.sketch.Sketch._
import tinaviz.scene.Scene
import tinaviz.util.Vector._
import actors.Actor

/**
 * 
 */
class Pipeline(val actor:Actor) extends node.util.Actor {

  start

  var nextState = 'output
  var data = new Graph()
  var sketch = new Sketch()
  
  def act() {
    while(true) {
      loop {
        react {
          // reset
          case g:Graph =>
            //println("we can run the full graph..")
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
            // println("running layout")
            runLayout
            //nextState = 'layout
            //val output = runLayout
            //cache += 'layout -> output
            //println("sending data back to the actor")

            sketch.overwrite(data)
            // println("  Renderer: done complete compilation of scene..")
            actor ! (sketch:Scene)
           
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
    val nbNodes = data.get[Int]("nbNodes")
    val barycenter = data.get[(Double,Double)]("baryCenter")

    val GRAVITY = data.get[Double]("layout.gravity")// stronger means faster!
    val ATTRACTION = data.get[Double]("layout.attraction")
    val REPULSION = data.get[Double]("layout.repulsion")// (if (nbNodes > 0) nbNodes else 1)// should be divided by the nb of edges


    //println("running forceVector on "+nbNodes+" nodes")

    val positions = data.position.zipWithIndex map {
      case (p1,i) =>
        var force = (0.0,0.0).computeForce(GRAVITY, barycenter)
        data.position.zipWithIndex map {
          case (p2,j)=>
            val p2inDegree = data inDegree j
            val p2outDegree = data outDegree j
            // todo: attract less if too close (will work for both gravity and node attraction)

            if (data.hasAnyLink(i,j)) {
              force += p1.computeForce(ATTRACTION, p2)
            } else {
              force -= p1.computeForceLimiter(REPULSION, p2)
            }
        }
        p1 + force
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



  def transformColumn[T](column:String,filter: T => T) = {
    var dataArray = data.getArray[T](column)
    dataArray.foreach{
      case entry =>
        

    }
  }
  def filterNodesByColumn[T](column:String,filter: T => Boolean) = {

    data.getArray[T](column).zipWithIndex foreach{
      case (entry,i) =>
        // if we need to keep the element,
        if (filter(entry)) {
          // we need to resize all arrays
          // then update all links to use new IDs
          var startingPoint = i
        }
    }
    //g.
    //new Graph(graph.nodes.filter {_.attributes(key).equals(value)},

    // graph)
  }
    
}
