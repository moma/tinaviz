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

  // used for original graph and global vars
  var data = new Graph()

  var categoryCache = new Graph()
  var layoutCache = new Graph()
  var sketch = new Sketch()
  
  def act() {
    while(true) {
      loop {
        react {
          // reset
          case g:Graph =>
            //println("we can run the full graph..")
            data = g
            categoryCache = applyCategory(data)

            //cache += 'input -> graph
            //self ! 'colors
            // self
          case (key:String, value:Any) =>
            println("updating graph attribute "+key+" -> "+value)
            data += key -> value
            self ! key
            
          case "category" =>
            println("categoryCache = applyCategory(data)")
            categoryCache = applyCategory(data)

          case "frameRate" =>
            println("layoutCache = applyLayout(layoutCache)")
            layoutCache = applyLayout(layoutCache)
            println("")
            sketch.update(layoutCache)
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



  def applyCategory(g:Graph) = {
    val category = g.get[String]("category")
    g + ("visible" -> (g.category.zipWithIndex map {
          case (cat,i) => (g.visible(i) && g.equals(category))
        }))
  }
  /**
   * apply a force vector algorithm on the graph
   */
  def applyLayout(g:Graph) = {
    val nbNodes = g.get[Int]("nbNodes")
    val barycenter = g.get[(Double,Double)]("baryCenter")

    val GRAVITY = g.get[Double]("layout.gravity")// stronger means faster!
    val ATTRACTION = g.get[Double]("layout.attraction")
    val REPULSION = g.get[Double]("layout.repulsion")// (if (nbNodes > 0) nbNodes else 1)// should be divided by the nb of edges

    //println("running forceVector on "+nbNodes+" nodes")

    val positions = g.position.zipWithIndex map {
      case (p1,i) =>
        var force = (0.0,0.0).computeForce(GRAVITY, barycenter)
        g.position.zipWithIndex map {
          case (p2,j)=>
            val p2inDegree = data inDegree j
            val p2outDegree = data outDegree j
            // todo: attract less if too close (will work for both gravity and node attraction)

            if (g.hasAnyLink(i,j)) {
              force += p1.computeForce(ATTRACTION, p2)
            } else {
              force -= p1.computeForceLimiter(REPULSION, p2)
            }
        }
        p1 + force
    }
    // TODO possible optimization: give some metrics
    g + ("position" -> positions)
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
    dataArray.foreach {
      case entry =>
        

    }
  }
 
  
}
