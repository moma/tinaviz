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
import tinaviz.io.Browser
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

  var framedelay = 200
  //var busy = false


  def act() {

    var next: Long = 0
    val me = self

    val sched = Executors.newSingleThreadScheduledExecutor()

    def schedule(msg: Any, ms: Long) = {
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

    schedule('ping, 500)

    while (true) {
      receive {
        // reset
        case g: Graph =>
          data = g
          self ! "filter.node.category" -> data.get[String]("filter.node.category")

        case ('getNodeAttributes, uuid: String) =>
          println("Server: asked for 'getNodeAttributes " + uuid)
          reply(data.lessAttributes(uuid))

        case ('getNeighbourhood, view: String, todoList: List[Any]) =>

        /*
       todoList.foreach{
         case x =>
         println("  - x: "+x)
       } */
          val result = (view match {
            case "meso" => layoutCache
            case any => data
          }).selectionNeighbours
          println("built selection neighbours: " + result)
          reply(result)

        case ('getNodes, view: String, category: String) =>
          println("Server: asked for 'getNodes " + view + " " + category)
          val all = (view match {
            case "meso" => layoutCache
            case any => data
          }).allNodes
          val result = if (category.equalsIgnoreCase("none")) {
            all
          } else {
            all.filter {
              case (uuid, attributes) => attributes("category").asInstanceOf[String].equals(category)
            }
          }
          reply(result)

        case "recenter" =>
          println("recentering now..")
          layoutCache = layoutCache.recenter
          updateScreen

        case ("camera.mouse", kind: Symbol, side: Symbol, count: Symbol, position: (Double, Double)) =>

        // sub routines to convert between screen and model coordinates
          val cz = layoutCache.cameraZoom
          val cp = layoutCache.cameraPosition
          //def modelPosition(p:(Int,Int)) : (Double,Double) = modelPosition(p._1,p._2)
          //def modelPosition(x:Int,y:Int) : (Double,Double) = modelPosition(x,y)
          def model2screen(p: (Double, Double)): (Int, Int) = (((p._1 + cp._1) * cz).toInt, ((p._2 + cp._2) * cz).toInt)
          def screen2model(p: (Double, Double)): (Double, Double) = ((p._1 - cp._1) / cz, (p._2 - cp._2) / cz)
          val o = screen2model(position)
          val sr = layoutCache.get[Double]("selectionRadius")
          val r = (sr / cz) / 2.0

          // (i) / cz) / 2.0

          //val mop = screen2model(onScreen)
          //println("mouse in screen: " + position + "    mouse in model: " + o)
          //println("selection radius: " + sr + "     scaled: " + r)
          kind match {
            case 'Click =>
              var in = false
              // TODO a selection counter
              layoutCache = layoutCache + ("selected" -> layoutCache.selected.zipWithIndex.map {
                case (before, i) =>
                  val touched = (layoutCache.position(i).dist(o) <= r)
                  if (touched) in = true
                  (before, touched)
              }.map {
                case (before, touched) =>
                  if (touched) {
                    !before
                  } else {
                    //before
                    // we don't touch a thing, unless nothing was selected at all (we reset everything in this case)
                    //
                    //
                   if (in) before else false
                  }
              }.toArray)
              // get the current selection with less attributes
              val selection = layoutCache.selectionAttributes // a List of selection
              // todo: update everything

              Browser ! "_callbackSelectionChanged" -> (selection, side match {
                case 'Left => "left"
                case 'Right => "right"
                case any => "none"
              })

              self ! "filter.view" -> data.get[String]("filter.view")

            case 'Drag =>
            val pause = try {
              data.get[Boolean]("pause")
            } catch {
              case x => true
            }
            //self ! "pause" -> true
            case 'Release =>
            //pauseBugger = false
            //self ! "pause" -> pauseBuffer
            case any =>
          }


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
            case "filter.view" =>
            //println("filter.view")
              categoryCache = data.updatePosition(layoutCache)
              categoryCache = applyCategory(categoryCache)
              categoryCache = applyWeightToSize(categoryCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = edgeWeightCache
              updateScreen

            case "filter.node.category" =>
            //println("filter.node.category")
              categoryCache = data.updatePosition(layoutCache)
              categoryCache = applyCategory(categoryCache)
              categoryCache = applyWeightToSize(categoryCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = edgeWeightCache
              updateScreen
            case "filter.node.weight" =>
            //println("nodeWeightCache = applyNodeWeight(categoryCache)")
              categoryCache = categoryCache.updatePosition(layoutCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = edgeWeightCache
              updateScreen
            case "filter.edge.weight" =>
            //println("edgeWeightCache = applyEdgeWeight(nodeWeightCache)")
              categoryCache = categoryCache.updatePosition(layoutCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = edgeWeightCache
              updateScreen
            case "filter.node.size" =>
            //println("categoryCache = applyWeightToSize(categoryCache)")
              categoryCache = categoryCache.updatePosition(layoutCache)
              categoryCache = applyWeightToSize(categoryCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = edgeWeightCache
              updateScreen

            //case "frameRate" =>
            //  layoutCache = applyLayout(layoutCache)
            //  updateScreen
            case any =>
          // we don't need to update the scene for other attributes
          }

        case ('ping, old: Long) =>
          val pause = try {
            data.get[Boolean]("pause")
          } catch {
            case x => true
          }
          if (!pause) {
            //println("pingu")
            layoutCache = applyLayout(layoutCache)
            updateScreen
            val d = (Platform.currentTime.toLong - old.toLong).toInt
            //println("d:" + d)
            if (d < framedelay) {
              // we are in advance
              //println("we are in advance, adjusting next frame to "+(-d))
              schedule('ping, framedelay - d)
            } else {
              // on time, if not in late, schedule now!
              //println("we are on time, schedule for right now")
              schedule('ping, 0)
            }
            //Scheduler.schedule( { Actor.actor { me ! 'ping }; () }, 100000L)
          } else {
            schedule('ping, framedelay)
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


  /**
   * Do some pre-processing, then send the final scene to the View
   * TODO: keep the Graph?
   */
  def updateScreen {
    // TODO: do that in another Actor, which will reply directly to our master
    val graph = layoutCache
    val g = graph + ("links" -> graph.links.zipWithIndex.map {
      case (links, i) =>
        links.filter {
          case (j, weight) =>
          // in the case of mutual link, we have a bit of work to remove the link
            if (graph.hasThisLink(j, i)) {

              // the bigger win
              if (graph.weight(i) > graph.weight(j)) {
                true
              } else if (graph.weight(i) < graph.weight(j)) {
                false

                // in the case of equal weight (eg. in Documents), we fall back to label comparison
              } else {

                // were the bigger win
                if (graph.label(i).compareTo(graph.label(j)) > 0) {
                  true
                } else {
                  false
                }
              }

              // in the case of non-mutual link (directed), there is nothing to do; we keep the link
            } else {
              true
            }
        }

    }.toArray)
    sketch.update(g)
    val msg = (g, sketch: Scene)
    actor ! msg
  }

  def applyCategory(g: Graph): Graph = {
    //println("applyCategory: "+g.debugStats)
    if (g.nbNodes == 0) return g
    var removeMe = Set.empty[Int]
    val category = g.currentCategory
    g.get[String]("filter.view") match {
      case "macro" =>
        g.category.zipWithIndex map {
          case (cat, i) =>
            if (!g.currentCategory.equalsIgnoreCase(cat)) {
              removeMe += i
            }
        }

      case "meso" =>
      //println("\n\nfiltering the meso view: "+g.debugStats)
        g.selected.zipWithIndex foreach {
          case (f, i) => if (!f) {

            // remove the node which is not in our category
            if (!g.currentCategory.equalsIgnoreCase(g.category(i))) {
              removeMe += i
            } else {
              var keepThat = false
              // we remove nodes not connected to the selection
              g.selection.foreach {
                case j => if (g.hasAnyLink(i, j)) keepThat = true
              }
              if (!keepThat) removeMe += i
            }
          }
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
   *  apply a force vector algorithm on the graph
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
              force += (p1.computeForce(ATTRACTION * cooling, p2))
            } else {
              // if (doIt) {
              force -= (p1.computeForceLimiter(REPULSION * cooling, p2))
              // }
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
