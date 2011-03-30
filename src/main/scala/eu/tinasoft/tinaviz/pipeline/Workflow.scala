/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.pipeline

import org.daizoru._
import eu.tinasoft._
import tinaviz.graph._
import tinaviz.Main
import tinaviz.Server
import tinaviz.io.json.Json
import tinaviz.io.Browser
import tinaviz.util.Vector._
import tinaviz.util.Maths
import tinaviz.layout._
import actors.Actor
import compat.Platform

/**
 *
 */
object Workflow extends node.util.Actor {

  def act() {

    while (true) {
      receive {

        case 'exit =>
          println("Workflow: exiting")
          exit()

        // The Layout Actor is a bit special: he is a slave to the Workflow. Here are its functions.
        case ('setLayout,g:Graph) =>
          //println("Workflow: setLayout("+g.nbNodes+")")
          // do some kin of "morphing" by merging latest computed coordinates inside the current Input graph
          Pipeline.setOutput(
            Pipeline.output.updatePositionWithCategory(
              g
            )
          )
        case 'getLayout =>
          //println("Workflow: getLayout ("+Pipeline.output.nbNodes+")")
          reply(Pipeline.output)

        case 'graphImported =>
              println("Workflow: graphImported.. warming filters up")
              println("Workflow: Pipeline.input.nbNodes: "+Pipeline.input.nbNodes)
              Pipeline.setCategoryCache(Filters.weightToSize(Pipeline.input))
              Pipeline.setNodeWeightCache(Filters.nodeWeight(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)))
              println("Workflow: Pipeline.output.nbNodes: "+Pipeline.output.nbNodes)

        case ('getNodeAttributes, uuid: String) =>
          println("Workflow: asked for 'getNodeAttributes (on INPUT GRAPH) of " + uuid)
          reply(Pipeline.input.lessAttributes(uuid))

        case ('getNeighbourhood, view: String, todoList: List[String]) =>
          val in = Pipeline.input
          val out = Pipeline.output
          val container = (view match {
            case "meso" => out
            case any => in
          })
          val neighbourList = Map(todoList.zipWithIndex: _*).map {
            case (uuid, i) => (uuid, container.neighbours(container.id(uuid)))
          }
          val nodeList = out.selectionUUID.toList


          //System.out.println("calling callback with this data: " + (nodeList, neighbourList))
          Browser ! "_callbackGetNeighbourhood" -> (nodeList, neighbourList)

        case ('getNeighbourhood, view: String, "selection") =>
          val in = Pipeline.input
          val out = Pipeline.output
          val container = (view match {
            case "meso" => {
              //println("taking the current (bad) neighbourhood graph")
              out
            }
            case any => {
              //println("taking neighbourhood from original (good) data graph")
              in
            }
          })
          //val neighbourListTmp = layoutCache.selectionUUID.map { case uuid => (uuid,container.neighbours(container.id(uuid))) }
          val nodeList = out.selectionUUID.toList
          val neighbourList = Map(out.selectionUUID.zipWithIndex: _*).map {
            case (uuid, i) => (uuid, container.neighbours(container.id(uuid)))
          }
          //System.out.println("calling callback with this data: " + (nodeList, neighbourList))
          Browser ! "_callbackGetNeighbourhood" -> (nodeList, neighbourList)


        case ('getNodes, view: String, category: String) =>
        //println("Server: asked for 'getNodes " + view + " " + category)
          val in = Pipeline.input
          val out = Pipeline.output
          val all = (view match {
            case "meso" => out
            case any => in
          }).allNodes
          val result = if (category.equalsIgnoreCase("none")) {
            all
          } else {
            all.filter {
              case (uuid, attributes) => attributes("category").asInstanceOf[String].equals(category)
            }
          }
          reply(result)

        case ("select", uuidList: List[String]) =>
        //println("selecting nodes: '" + uuidList + "'")
          val in = Pipeline.input
          val out = Pipeline.output
          Pipeline.setOutput(
          if (uuidList.size == 0) {
              out + ("selected" -> out.selected.map(c => false))
          } else {
              out + ("selected" -> out.uuid.zipWithIndex.map {
                case (_uuid, i) =>
                // quick & dirty..I  don't remember name of a better function, and I need to release tomorrow
                  var found = false
                  uuidList.foreach {
                    case uuid => if (_uuid equals uuid) found = true
                  }
                  val res = if (found) true else out.selected(i)
                  println("match: " + res)
                  res
              })
          })
          Browser ! "_callbackSelectionChanged" -> (Pipeline.output.selectionAttributes, "left")
          self ! "filter.view" -> in.currentView

        case ("select", uuid: String) =>
          val out = Pipeline.output
          Pipeline.setOutput(
            if (uuid == null | (uuid.equals(" ") || uuid.isEmpty)) {
              out + ("selected" -> out.selected.map(c => false))
            } else {
              out + ("selected" -> out.uuid.zipWithIndex.map {
                case (_uuid, i) =>
                  val res = if (_uuid equals uuid) true else out.selected(i)
                  res
              })
            })

          Browser ! "_callbackSelectionChanged" -> (Pipeline.output.selectionAttributes, "left")
          //self ! "filter.view" -> Pipeline.input.currentView

        case ("selectByPattern", pattern: String) =>
          val out = Pipeline.output
          Pipeline.setOutput(
            if (pattern == null | (pattern.equals(" ") || pattern.isEmpty)) {
              out + ("selected" -> out.selected.map(c => false))
            } else {
              out + ("selected" -> out.label.zipWithIndex.map {
                case (label, i) => if (label.toLowerCase contains pattern.toLowerCase) true else out.selected(i)
              })
            }
          )
          Browser ! "_callbackSelectionChanged" -> (Pipeline.output.selectionAttributes, "left")
          self ! "filter.view" -> Pipeline.input.currentView

        case ("highlightByPattern", pattern: String) =>
          val in = Pipeline.input
          val out = Pipeline.output
          Pipeline.setOutput(out + ("highlighted" -> out.label.map {
            case label => if (pattern == null | pattern.isEmpty) false else (label.toLowerCase contains pattern.toLowerCase)
          }))
          //Browser ! "_callbackSelectionChanged" -> "left"
          self ! "filter.view" -> in.currentView // will automatically update the highlight section

        case "recenter" =>
        //println("recentering now..")
          Pipeline.setOutput(Functions.recenter(Pipeline.output))


        case ("camera.mouse", kind: Symbol, side: Symbol, count: Symbol, position: (Double, Double)) =>
          val out = Pipeline.output
          val (cz,cp, sr) = (out.cameraZoom, out.cameraPosition, out.selectionRadius)
          def model2screen(p: (Double, Double)): (Int, Int) = (((p._1 + cp._1) * cz).toInt, ((p._2 + cp._2) * cz).toInt)
          def screen2model(p: (Double, Double)): (Double, Double) = ((p._1 - cp._1) / cz, (p._2 - cp._2) / cz)
          val o = screen2model(position)
          val r = (sr / cz)
          kind match {
            case 'Move =>
              var changed = false
              // TODO a selection counter
              val out2 = out + ("highlighted" -> out.highlighted.zipWithIndex.map {
                case (before, i) =>
                  val l = out.size(i) // maths hack
                  val p = out.position(i)
                  val ggg = (p.isInRange(o, r) || p.isInRange(o, l + (l / 2.0))) // maths
                  if (ggg != before) changed = true
                  ggg
              }.toArray)
              if (changed) Pipeline.setOutput(out2)

            case 'Click =>
              var in = false
              // TODO a selection counter
              val out2 = out + ("selected" -> out.selected.zipWithIndex.map {
                case (before, i) =>
                  val l = out.size(i) // maths hack
                  val p = out.position(i)
                  val touched = (p.isInRange(o, r) || p.isInRange(o, l + (l / 2.0))) // maths
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
                    if (out.currentView.equalsIgnoreCase("macro")) {

                      // if (in) before else false  // uncomment to enable unselection with single click
                      before

                    } else {
                      count match {
                        case 'Simple => before
                        case 'Double => false
                      }
                    }
                  }
              }.toArray)


              Browser ! "_callbackSelectionChanged" -> (out2.selectionAttributes, side match {
                case 'Left => "left"
                case 'Right => "right"
                case any => "none"
              })

              // check if we need to recompute the meso field
              count match {
                case 'Double =>
                  if (in) {

                    if (out.currentView.equalsIgnoreCase("macro"))
                       Browser ! "_callbackSelectionChanged" -> "meso"

                    self ! "filter.view" -> "meso"

                  } else {
                    // zoom?
                  }
                case 'Simple =>
                  //println("Workflow: updating view (is it OK?)")
                  //self ! "filter.view" -> out2.currentView
              }

            Pipeline.setOutput(out2)

            case 'Drag =>
            val pause = try {
              Pipeline.input.pause
            } catch {
              case x => true
            }
            //self ! "pause" -> true
            case 'Release =>
            //pauseBugger = false
            //self ! "pause" -> pauseBuffer
            case any =>
          }

        case (key: String, value: Any) =>
           Pipeline.applyKey(key, value)

          key match {
            case "filter.view" =>
            //println("filter.view")
              val out = Pipeline.output
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Filters.category(Pipeline.input)))
              Pipeline.setNodeWeightCache(Filters.nodeWeight(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)))
            case "filter.node.category" =>
            //println("filter.node.category")
              println("Workflow: category updated")
              val out = Pipeline.output
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Filters.category(Pipeline.input)))
              Pipeline.setNodeWeightCache(Filters.nodeWeight(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)))
            case "filter.a.node.weight" =>
            //println("nodeWeightCache = Filters.nodeWeight(categoryCache)")
              println("Workflow: document weight updated")
              val out = Pipeline.output
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Pipeline.categoryCache.updatePositionWithCategory(out)))
              Pipeline.setNodeWeightCache(Filters.nodeWeight(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)))
            case "filter.a.edge.weight" =>
            //println("edgeWeightCache = Filters.edgeWeight(nodeWeightCache)")
              println("Workflow: document edge weight updated")
              val out = Pipeline.output
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Pipeline.categoryCache.updatePositionWithCategory(out)))
              Pipeline.setNodeWeightCache(Filters.nodeWeight(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)))
            case "filter.b.node.weight" =>
            //println("nodeWeightCache = Filters.nodeWeight(categoryCache)")
              val out = Pipeline.output
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Pipeline.categoryCache.updatePositionWithCategory(out)))
              Pipeline.setNodeWeightCache(Filters.nodeWeight(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)))
            case "filter.b.edge.weight" =>
            //println("edgeWeightCache = Filters.edgeWeight(nodeWeightCache)")
              val out = Pipeline.output
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Pipeline.categoryCache.updatePositionWithCategory(out)))
              Pipeline.setNodeWeightCache(Filters.nodeWeight(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)))
            case "filter.a.node.size" =>
            //println("categoryCache = Filters.weightToSize(categoryCache)")
              println("Workflow: document size updated")
              val out = Pipeline.output
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Pipeline.categoryCache.updatePositionWithCategory(out)))
              Pipeline.setNodeWeightCache(Filters.nodeWeight(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)))
            case "filter.b.node.size" =>
            //println("categoryCache = Filters.weightToSize(categoryCache)")
              println("Workflow: ngram size updated")
              val out = Pipeline.output
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Pipeline.categoryCache.updatePositionWithCategory(out)))
              Pipeline.setNodeWeightCache(Filters.nodeWeight(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)))
            case any => // we don't need to update the scene for other attributes
          }

        case s: String =>
        case msg => println("Workflow: unknow msg: " + msg)
      }
    }
  }


}
