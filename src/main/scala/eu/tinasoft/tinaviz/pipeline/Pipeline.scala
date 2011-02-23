/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.pipeline

import org.daizoru._
import eu.tinasoft._
import tinaviz.graph.Graph
import tinaviz.graph.GraphMetrics
import tinaviz.graph.GraphFunctions
import tinaviz.Server
import tinaviz.sketch.Sketch
import tinaviz.sketch.Sketch._
import tinaviz.scene.Scene
import tinaviz.io.Browser
import tinaviz.util.Vector._
import tinaviz.util.Maths
import java.util.concurrent.{ScheduledFuture, TimeUnit, Executors}
import actors.Actor
import compat.Platform

/**
 *
 */
object Pipeline extends node.util.Actor {

  start

  var nextState = 'output
  var data = new Graph()
  var categoryCache = new Graph()
  var nodeWeightCache = new Graph()
  var edgeWeightCache = new Graph()
  var layoutCache = new Graph()
  var sketch = new Sketch()
  var scene = new Scene()
  var framedelay = 200

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

        case('selectByPattern,pattern:String) =>
            layoutCache = layoutCache + ("selected" -> layoutCache.label.map {
                case label => if (pattern.isEmpty) false else (label contains pattern)
            })
          
            val selection = layoutCache.selectionAttributes
            // todo: update everything

            Browser ! "_callbackSelectionChanged" -> (selection, "left")

            self ! "filter.view" -> data.get[String]("filter.view")
            
        case('highlightByPattern,pattern:String) =>
            layoutCache = layoutCache + ("highlighted" -> layoutCache.label.map {
                case label => if (pattern.isEmpty) false else (label contains pattern)
            })
            //Browser ! "_callbackSelectionChanged" -> "left"
            self ! "filter.view" -> data.get[String]("filter.view")
          
        case "recenter" =>
          println("recentering now..")
          layoutCache = GraphFunctions.recenter(layoutCache)
          updateScreen

        case ("camera.mouse", kind: Symbol, side: Symbol, count: Symbol, position: (Double, Double)) =>
          val cz = layoutCache.cameraZoom
          val cp = layoutCache.cameraPosition
          def model2screen(p: (Double, Double)): (Int, Int) = (((p._1 + cp._1) * cz).toInt, ((p._2 + cp._2) * cz).toInt)
          def screen2model(p: (Double, Double)): (Double, Double) = ((p._1 - cp._1) / cz, (p._2 - cp._2) / cz)
          val o = screen2model(position)
          val sr = layoutCache.get[Double]("selectionRadius")
          val r = (sr / cz) / 2.0
          kind match {
            case 'Move =>
                      var changed = false
                      // TODO a selection counter
                      layoutCache = layoutCache + ("highlighted" -> layoutCache.highlighted.zipWithIndex.map {
                        case (before, i) =>
                          val l = layoutCache.size(i)   // maths hack
                          val p = layoutCache.position(i)
                          val ggg = (p.isInRange(o,r) || p.isInRange(o,l+(l/2.0))) // maths
                          if (ggg != before) changed = true
                          ggg
                      }.toArray)
                      if (changed) updateScreen

            case 'Click =>
              var in = false
              // TODO a selection counter
              layoutCache = layoutCache + ("selected" -> layoutCache.selected.zipWithIndex.map {
                case (before, i) =>
                  val l = layoutCache.size(i)   // maths hack
                  val p = layoutCache.position(i)
                  val touched = (p.isInRange(o,r) || p.isInRange(o,l+(l/2.0))) // maths
                  if (touched) in = true
                  (before, touched)
              }.map {
                case (before, touched) =>
                  if (touched) {
                    !before
                  } else {
                   if (layoutCache.get[String]("filter.view").equalsIgnoreCase("macro")) {
                     if (in) before else false
                   } else {
                      before
                   }
                  }
              }.toArray)
              val selection = layoutCache.selectionAttributes
              // todo: update everything

              Browser ! "_callbackSelectionChanged" -> (selection, side match {
                case 'Left => "left"
                case 'Right => "right"
                case any => "none"
              })

              self ! "filter.view" -> (count match {
                case 'Simple => data.get[String]("filter.view")
                case 'Double => "meso"
              })

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
          println("selecting node: '"+uuid+"'")
          if (uuid.equals(" ") || uuid.isEmpty) {
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
            case any => // we don't need to update the scene for other attributes
          }

        case ('ping, old: Long) =>
          val pause = try {
            data.get[Boolean]("pause")
          } catch {
            case x => true
          }
          if (!pause) {
            layoutCache = applyLayout(layoutCache)
            updateScreen
            val d = (Platform.currentTime.toLong - old.toLong).toInt
            //println("d:" + d)
            if (d < framedelay) {
              //println("we are in advance, adjusting next frame to "+(-d))
              schedule('ping, framedelay - d)
            } else {
              //println("we are on time, schedule for right now")
              schedule('ping, 0)
            }
          } else {
            schedule('ping, framedelay)
          }
        case s: String =>
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
    val f = graph + ("links" -> graph.links.zipWithIndex.map {
      case (links, i) =>
        links.filter {
          case (j, weight) =>
          // in the case of mutual link, we have a bit of work to remove the link
            if (graph.hasThisLink(j, i)) {
              // if i is bigger than j, we keep
              GraphFunctions.isBiggerThan(graph,i,j)
              // in the case of non-mutual link (directed), there is nothing to do; we keep the link
            } else {
              true
            }
        }

    }.toArray)
    
    sketch.update(f)
    val msg = (f, sketch: Scene)
    Server ! msg
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
    var removeMe = Set.empty[Int]
    g.weight.zipWithIndex.map { case (weight, i) => if (!(range._1 <= weight && weight <= range._2)) removeMe += i }
    val h = g.remove(removeMe)
    h + ("activity" -> GraphMetrics.activity(h,g))
  }

  def applyEdgeWeight(g: Graph): Graph = {
    if (g.nbNodes == 0) return g
    val range = Maths.map(
      g.get[(Double, Double)]("filter.edge.weight"),
      (0.0, 1.0),
      (g.get[Double]("minEdgeWeight"), g.get[Double]("maxEdgeWeight")))
    //println("applyEdgeWeight: " + range + " (" + g.get[(Double, Double)]("filter.edge.weight") + ")")
    val newLinks = g.links map {
      case links => links.filter { case (id, weight) => (range._1 <= weight && weight <= range._2) }
    }
    val h = g + ("links" -> newLinks)
   h + ("activity" -> GraphMetrics.activity(h,g))
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
    val cooling =1.0
    
    //var activ = 0.0
    val positions = g.position.zipWithIndex map {
      case (p1, i) =>
        var force = (0.0,0.0)
        force += p1.computeLessForce(30.0 * cooling, (0.0,0.0))
        g.position.zipWithIndex foreach {
          case (p2, j) =>
            val p2inDegree = data inDegree j
            val p2outDegree = data outDegree j
            val doIt = Maths.randomBool

            
            // todo: attract less if too close (will work for both gravity and node attraction)
            if (true || g.hasAnyLink(i, j)) {
              force += p1.computeLessForce(ATTRACTION * cooling, p2)
            } else {
              // if (doIt) {
              force -= p1.computeLessForce(REPULSION * cooling, p2)
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
       println("p1: "+p1+" abs_f1: "+math.abs(force._1)+" abs_f2 "+math.abs(force._2)+" p1': "+(p1 + force))
       //(math.max(0.01, math.abs(force._1)), math.max(0.01, math.abs(force._1)))
       //activ += (force._1+force._2)
       //

        p1 + (if (math.abs(force._1) > 0.01) force._1 else { println("cancelling force in X "); 0.0 },
              if (math.abs(force._2) > 0.01) force._2 else { println("cancelling force in Y "); 0.0 })
    }
   
    //println("activity: "+activity)
    // TODO possible optimization: give some metrics
    val h = g + ("position" -> positions)
    h //+ ("activity" -> activity)
  }

  def transformColumn[T](column: String, filter: T => T) = {
    var dataArray = data.getArray[T](column)
    dataArray.foreach {
      case entry =>
    }
  }

}
