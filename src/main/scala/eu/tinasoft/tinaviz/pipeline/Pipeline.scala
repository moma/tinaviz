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
import tinaviz.util.Maths
import java.util.concurrent.{ScheduledFuture, TimeUnit, Executors}

//import tinaviz.util.ActorPing

import actors.Actor

//import java.util.Date

import compat.Platform

//import actors.threadpool.{TimeUnit, Executors}


/**
 *
 */
class Pipeline(val actor: Actor) extends node.util.Actor {

  start

  var nextState = 'output

  // used for original graph and global vars
  var data = new Graph()

  var categoryCache = new Graph()
  var nodeWeightCache = new Graph()
  var edgeWeightCache = new Graph()
  var layoutCache = new Graph()
  var sketch = new Sketch()
  var scene = new Scene()

  //var busy = false


  def act() {

    val fps: Double = 5
    var next: Long = 0
    val me = self

    val sched = Executors.newSingleThreadScheduledExecutor()

    def schedule(msg: Any, ms: Long = 0) = {
      val ct = Platform.currentTime
      sched.schedule(new Runnable {
        def run = actors.Scheduler.execute({
          Actor.actor {
            me ! (msg, ct - ms)
          };
          ()
        })
      }, ms, TimeUnit.MILLISECONDS)
    }

    schedule('ping)

    while (true) {
      receive {
        // reset
        case g: Graph =>
          data = g
          self ! "filter.node.category" -> data.get[String]("filter.node.category")

        case ('getNodeAttributes,uuid:String) => reply(layoutCache.attributes(uuid))

        case ('getNodes,view:String,category:String) =>
          reply(layoutCache.allAttributes)

        case ("select", uuid: String) =>
          if (uuid.equals("")) {
            data += "selected" -> data.selected.map(c => false)
          } else {
            data += (data.id(uuid), "select", true)
          }
          self ! "filter.node.category" -> data.get[String]("filter.node.category")

        case (key: String, value: Any) =>
          //println("updating graph attribute " + key + " -> " + value)
          data += key -> value
          categoryCache += key -> value
          nodeWeightCache += key -> value
          edgeWeightCache += key -> value
          layoutCache += key -> value
          key match {
            case "filter.node.category" =>
              //println("categoryCache = applyCategory(data)")
              categoryCache = applyCategory(data)
              categoryCache = applyWeightToSize(categoryCache)
              categoryCache = categoryCache.updatePosition(layoutCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = edgeWeightCache
              sendScene
            case "filter.node.weight" =>
              //println("nodeWeightCache = applyNodeWeight(categoryCache)")
              categoryCache = categoryCache.updatePosition(layoutCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = edgeWeightCache
              sendScene
            case "filter.edge.weight" =>
              //println("edgeWeightCache = applyEdgeWeight(nodeWeightCache)")
              categoryCache = categoryCache.updatePosition(layoutCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = edgeWeightCache
              sendScene
            case "filter.node.size" =>
              //println("categoryCache = applyWeightToSize(categoryCache)")
              categoryCache = categoryCache.updatePosition(layoutCache)
              categoryCache = applyWeightToSize(categoryCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = edgeWeightCache
              sendScene

            //case "frameRate" =>
            //  layoutCache = applyLayout(layoutCache)
            //  sendScene
            case any =>
          // we don't need to update the scene for other attributes
          }

        case ('ping, old: Long) =>
          val pause = try { data.get[Boolean]("pause") } catch { case x => true }
          if (!pause) {
            //println("pingu")
            layoutCache = applyLayout(layoutCache)
            sendScene
            val d = (Platform.currentTime.toLong - old.toLong).toInt
            //println("d:" + d)
            if (d < 200) {
              // we are in advance
              //println("we are in advance, adjusting next frame to "+(-d))
              schedule('ping, 200 - d)
            } else {
              // on time, if not in late, schedule now!
              //println("we are on time, schedule for right now")
              schedule('ping)
            }
            //Scheduler.schedule( { Actor.actor { me ! 'ping }; () }, 100000L)
          } else {
               schedule('ping, 200)
          }


        /*
val now = (new Date).getTime
if (next > now) {
  println("waiting")
  Thread.sleep(next - now)
}
next = (now.toDouble + 1000.0 / fps).toLong
println("self-pinging")
self ! 'ping      */

        case s: String =>
        // ignore
        //
        //case
        case msg => println("unknow msg: " + msg)
      }

    }
  }

  def sendScene {
    sketch.update(layoutCache)
    scene = sketch: Scene
    actor ! scene
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

  /*
  def applyView(g:Graph) = {
    val view = g.get[String]("filter.view")
    if (view.equals("macro")) {
        
    } else {
    
    }
    var removeMe = Set.empty[Int]
    g.category.zipWithIndex map {
      case (cat,i) =>
        if (cat.equals(category)) {
          removeMe += i
        }
    }
    var h = g.remove(removeMe)
    h
  }*/

  def applyCategory(g: Graph): Graph = {
    if (g.nbNodes == 0) return g
    val category = g.get[String]("filter.node.category")
    //println("applyCategory: " + category)
    var removeMe = Set.empty[Int]
    g.category.zipWithIndex map {
      case (cat, i) =>
        if (cat.equals(category)) {
          removeMe += i
        }
    }
    g.remove(removeMe)
  }


  def applyNodeWeight(g: Graph): Graph = {
    if (g.nbNodes == 0) return g
    val range = Maths.map(
      g.get[(Double, Double)]("filter.node.weight"),
      (0.0, 1.0),
      (g.get[Double]("minNodeWeight"), g.get[Double]("maxNodeWeight")))
    //println("applyNodeWeight: " + range + " (" + g.get[(Double, Double)]("filter.node.weight") + ")")
    var removeMe = Set.empty[Int]
    g.weight.zipWithIndex.map {
      case (weight, i) =>
      //println("filtering " + weight + " : " + i)
        if (!(range._1 <= weight && weight <= range._2)) {
          removeMe += i
        }
    }
    val h = g.remove(removeMe)
    h.computeActivity(g) // todo should be done automatically when addind
  }

  def applyEdgeWeight(g: Graph): Graph = {
    if (g.nbNodes == 0) return g
    val range = Maths.map(
      g.get[(Double, Double)]("filter.edge.weight"),
      (0.0, 1.0),
      (g.get[Double]("minEdgeWeight"), g.get[Double]("maxEdgeWeight")))
    //println("applyEdgeWeight: " + range + " (" + g.get[(Double, Double)]("filter.edge.weight") + ")")
    val newLinks = g.links map {
      case links =>
        links.filter {
          case (id, weight) => (range._1 <= weight && weight <= range._2)
        }
    }
    val h = g + ("links" -> newLinks)
    h.computeActivity(g)
  }

  def applyWeightToSize(g: Graph): Graph = {
    if (g.nbNodes == 0) return g
    val ratio = 1.0 * g.get[Double]("filter.node.size")
    //println("applyWeightToSize: " + ratio)
    val newSize = g.weight map {
      case weight => weight * ratio
    }
    g + ("size" -> newSize)
  }

  /*
  def applyCategory(g:Graph) = {
  val category = g.get[String]("filter.category")
  g + ("visible" -> (g.category.zipWithIndex map {
  case (cat,i) =>
  (g.visible(i) && g.equals(category))
  }))
  }*/
  /**
   * apply a force vector algorithm on the graph
   */
  def applyLayout(g: Graph): Graph = {
    val nbNodes = g.nbNodes
    if (nbNodes == 0) return g
    val barycenter = (0.0, 0.0) //g.get[(Double, Double)]("baryCenter")
    val GRAVITY = g.get[Double]("layout.gravity") // stronger means faster!
    val ATTRACTION = g.get[Double]("layout.attraction")
    val REPULSION = g.get[Double]("layout.repulsion") // (if (nbNodes > 0) nbNodes else 1)// should be divided by the nb of edges

    //println("running forceVector on "+nbNodes+" nodes")


    //if (g.activity < 0.005) return g + ("activity" -> 0.0)
    val cooling = 1.0 //Maths.map(g.activity,(0.0,1.0),(0.900, 0.999))

    val positions = g.position.zipWithIndex map {
      case (p1, i) =>
        var force = (0.0, 0.0).computeForce(GRAVITY, barycenter)
        g.position.zipWithIndex map {
          case (p2, j) =>
            val p2inDegree = data inDegree j
            val p2outDegree = data outDegree j
            val doIt = Maths.randomBool

            // todo: attract less if too close (will work for both gravity and node attraction)
            if (g.hasAnyLink(i, j)) {
              force += p1.computeForce(ATTRACTION * cooling, p2)
            } else {
              if (doIt) {
                force -= p1.computeForceLimiter(REPULSION * cooling, p2)
              }
            }
        }

        // random repulse
        /*
          if (Maths.random() < 0.05f) {
               val theta = 2 * math.Pi * Maths.random()
               (((math.cos(theta) - math.sin(theta))) * desiredDist,
                ((math.cos(theta) + math.sin(theta))) * desiredDist)
        } else {
              p1 + force
        }*/
        p1 + force
    }
    // TODO possible optimization: give some metrics
    val h = g + ("position" -> positions)
    h.computeActivity(g)
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


  def transformColumn[T](column: String, filter: T => T) = {
    var dataArray = data.getArray[T](column)
    dataArray.foreach {
      case entry =>


    }
  }


}
