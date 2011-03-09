/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.pipeline

import org.daizoru._
import eu.tinasoft._
import tinaviz.graph.Graph
import tinaviz.graph.Metrics
import tinaviz.graph.Functions
import tinaviz.Server
import tinaviz.sketch.Sketch
import tinaviz.sketch.Sketch._
import tinaviz.scene.Scene
import tinaviz.io.Browser
import tinaviz.util.Vector._
import tinaviz.util.Maths
import eu.tinasoft.tinaviz.graph.Filters
import eu.tinasoft.tinaviz.graph.Layout
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
         categoryCache = applyCategory(data)
         categoryCache = applyWeightToSize(categoryCache)
         nodeWeightCache = applyNodeWeight(categoryCache)
         edgeWeightCache = applyEdgeWeight(nodeWeightCache)
         layoutCache = edgeWeightCache
         updateScreen

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

        case("selectByPattern",pattern:String) =>
          layoutCache = layoutCache + ("selected" -> layoutCache.label.map {
              case label => if (pattern == null | pattern.isEmpty) false else (label.toLowerCase contains pattern.toLowerCase)
            })
          
          val selection = layoutCache.selectionAttributes
          // todo: update everything

          Browser ! "_callbackSelectionChanged" -> (selection, "left")

          self ! "filter.view" -> data.get[String]("filter.view")
            
        case("highlightByPattern",pattern:String) =>
          layoutCache = layoutCache + ("highlighted" -> layoutCache.label.map {
              case label => if (pattern == null | pattern.isEmpty) false else (label.toLowerCase contains pattern.toLowerCase)
            })
          //Browser ! "_callbackSelectionChanged" -> "left"
          self ! "filter.view" -> data.get[String]("filter.view")
          
        case "recenter" =>
          println("recentering now..")
          layoutCache = Functions.recenter(layoutCache)
          updateScreen

        case ("camera.mouse", kind: Symbol, side: Symbol, count: Symbol, position: (Double, Double)) =>
          val cz = layoutCache.cameraZoom
          val cp = layoutCache.cameraPosition
          def model2screen(p: (Double, Double)): (Int, Int) = (((p._1 + cp._1) * cz).toInt, ((p._2 + cp._2) * cz).toInt)
          def screen2model(p: (Double, Double)): (Double, Double) = ((p._1 - cp._1) / cz, (p._2 - cp._2) / cz)
          val o = screen2model(position)
          val sr = layoutCache.get[Double]("selectionRadius")
          val r = (sr / cz)
          kind match {
            case 'Move =>
              var changed = false
              // TODO a selection counter
              layoutCache += ("highlighted" -> layoutCache.highlighted.zipWithIndex.map {
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
              layoutCache += ("selected" -> layoutCache.selected.zipWithIndex.map {
                  case (before, i) =>
                    val l = layoutCache.size(i)   // maths hack
                    val p = layoutCache.position(i)
                    val touched = (p.isInRange(o,r) || p.isInRange(o,l+(l/2.0))) // maths
                    if (touched) in = true
                    (before, touched)
                }.map {
                  case (before, touched) =>
                    if (touched) {
                      count match {
                        case 'Simple => !before
                        case 'Double => true
                      }
                    } else {
                      if (layoutCache.get[String]("filter.view").equalsIgnoreCase("macro")) {
                        if (in) before else false
                      } else {
                      count match {
                        case 'Simple => before
                        case 'Double => false
                      }
                      }
                    }
                }.toArray)
              Browser ! "_callbackSelectionChanged" -> (layoutCache.selectionAttributes, side match {
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

          if (uuid == null | (uuid.equals(" ") || uuid.isEmpty)) {
            val t = data.selected.map(c => false)
            // TODO refactor this, seems to fail from time to time
            data        += ("selected" -> data.selected.map(c => false))
            layoutCache += ("selected" -> layoutCache.selected.map(c => false))
          } else  {
            layoutCache += (data.id(uuid), "select", true)
          }
          println("updating selection")
          Browser ! "_callbackSelectionChanged" -> (layoutCache.selectionAttributes, "left")
          self ! "filter.view" -> data.get[String]("filter.view")

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
              data = data.updatePositionWithCategory(layoutCache)
              categoryCache = applyCategory(data)
              categoryCache = applyWeightToSize(categoryCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = applyCategory(edgeWeightCache)
              updateScreen
            case "filter.node.category" =>
              //println("filter.node.category")
              println("we store the positions")
              data = data.updatePositionWithCategory(layoutCache)
              categoryCache = applyCategory(data)
              categoryCache = applyWeightToSize(categoryCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = applyCategory(edgeWeightCache)
              updateScreen
            case "filter.a.node.weight" =>
              //println("nodeWeightCache = applyNodeWeight(categoryCache)")
              data = data.updatePositionWithCategory(layoutCache)
              categoryCache = categoryCache.updatePositionWithCategory(layoutCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = applyCategory(edgeWeightCache)
              updateScreen
            case "filter.a.edge.weight" =>
              //println("edgeWeightCache = applyEdgeWeight(nodeWeightCache)")
              data = data.updatePositionWithCategory(layoutCache)
              categoryCache = categoryCache.updatePositionWithCategory(layoutCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = applyCategory(edgeWeightCache)
              updateScreen
           case "filter.b.node.weight" =>
              //println("nodeWeightCache = applyNodeWeight(categoryCache)")
              data = data.updatePositionWithCategory(layoutCache)
              categoryCache = categoryCache.updatePositionWithCategory(layoutCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = applyCategory(edgeWeightCache)
              updateScreen
            case "filter.b.edge.weight" =>
              //println("edgeWeightCache = applyEdgeWeight(nodeWeightCache)")
              data = data.updatePositionWithCategory(layoutCache)
              categoryCache = categoryCache.updatePositionWithCategory(layoutCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = applyCategory(edgeWeightCache)
              updateScreen
            case "filter.a.node.size" =>
              //println("categoryCache = applyWeightToSize(categoryCache)")
              data = data.updatePositionWithCategory(layoutCache)
              categoryCache = categoryCache.updatePositionWithCategory(layoutCache)
              categoryCache = applyWeightToSize(categoryCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = applyCategory(edgeWeightCache)
              updateScreen
            case "filter.b.node.size" =>
              //println("categoryCache = applyWeightToSize(categoryCache)")
              data = data.updatePositionWithCategory(layoutCache)
              categoryCache = categoryCache.updatePositionWithCategory(layoutCache)
              categoryCache = applyWeightToSize(categoryCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = applyCategory(edgeWeightCache)
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

  def genericWorkflow {
              //println("categoryCache = applyWeightToSize(categoryCache)")
              data = data.updatePositionWithCategory(layoutCache)
              categoryCache = categoryCache.updatePositionWithCategory(layoutCache)
              categoryCache = applyWeightToSize(categoryCache)
              nodeWeightCache = applyNodeWeight(categoryCache)
              edgeWeightCache = applyEdgeWeight(nodeWeightCache)
              layoutCache = applyCategory(edgeWeightCache)
              updateScreen
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
                Functions.isBiggerThan(graph,i,j)
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


  def applyCategory(g: Graph) = Filters.category(g)
  def applyNodeWeight(g: Graph) = Filters.nodeWeight(g)
  def applyEdgeWeight(g: Graph) = Filters.edgeWeight(g)
  def applyLayout(g: Graph) = Layout.layout(g)
  def applyWeightToSize(g: Graph): Graph = Filters.weightToSize(g)
  
  /*  */
  def transformColumn[T](column: String, filter: T => T) = {
    var dataArray = data.getArray[T](column)
    dataArray.foreach {
      case entry =>
    }
  }

}
