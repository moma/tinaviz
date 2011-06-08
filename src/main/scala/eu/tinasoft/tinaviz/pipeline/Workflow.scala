/************************************************************************
                                  Tinaviz
*************************************************************************
 This application is part of the Tinasoft project: http://tinasoft.eu
 Tinaviz main developer: julian.bilcke @ iscpif.fr  (twitter.com/flngr)

 Copyright (C) 2009-2011 CREA Lab, CNRS/Ecole Polytechnique UMR 7656 (Fr)

 This program is free software: you can redistribute it and/or modify it
 under the terms of the GNU General Public License as published by the
 Free Software Foundation, either version 3 of the License, or (at your
 option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program. If not, see <http://www.gnu.org/licenses/>.
************************************************************************/

package eu.tinasoft.tinaviz.pipeline

import eu.tinasoft._
import tinaviz.graph._
import tinaviz.Main
import tinaviz.Server
import tinaviz.io.json.Json
import tinaviz.io.Browser
import tinaviz.io.GEXF
import tinaviz.util.Vector._
import tinaviz.util.Maths
import tinaviz.layout._

import actors.Actor
import actors.Actor._

import compat.Platform
import com.lowagie.text.pdf.codec.Base64.OutputStream

/**
 *
 */
object Workflow extends Actor {

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
              Pipeline.setCategoryCache(Filters.weightToSize(Pipeline.input))
              Pipeline.setNodeWeightCache(Filters.nodeWeight2(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)).callbackNodeCountChanged)
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
         // println("selecting nodes: '" + uuidList + "'")
          val in = Pipeline.input
          val out = Pipeline.output
          val out2 = if (uuidList.size == 0) {
              Pipeline.setCategoryCache(Pipeline.categoryCache.clearSelection)
              in.clearSelection
              out.clearSelection
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
          }
          Pipeline.setOutput(out2.callbackSelectionChanged)
          Browser ! "_callbackSelectionChanged" -> (Pipeline.output.selectionAttributes, "left")
          self ! "filter.view" -> in.currentView

        case ("select", uuid: String) =>
          val in = Pipeline.input
          val out = Pipeline.output
          val out2 = if (uuid == null | (uuid.equals(" ") || uuid.isEmpty)) {
              Pipeline.setCategoryCache(Pipeline.categoryCache.clearSelection)
              in.clearSelection
              out.clearSelection
            } else {
              out + ("selected" -> out.uuid.zipWithIndex.map {
                case (_uuid, i) =>
                  val res = if (_uuid equals uuid) true else out.selected(i)
                  res
              })
            }
          Pipeline.setOutput(out2.callbackSelectionChanged)
          //println("calling Pipeline.output.updateSelectedWithCategory( g )")
          /*Pipeline.output.updateSelectedWithCategory(
              out2
          ) */
          //println("out2.selection.size: " + out2.selection.size)
          // println("Pipeline.output.size: " + Pipeline.output.size)
          Browser ! "_callbackSelectionChanged" -> (Pipeline.output.selectionAttributes, "left")
          //self ! "filter.view" -> Pipeline.input.currentView

        case ("selectByPattern", pattern: String) =>
          val in = Pipeline.input
          val out = Pipeline.output
          Pipeline.setOutput(
            (if (pattern == null | (pattern.equals(" ") || pattern.isEmpty)) {
              Pipeline.setCategoryCache(Pipeline.categoryCache.clearSelection)
             in.clearSelection
              out.clearSelection
            } else {
              out + ("selected" -> out.label.zipWithIndex.map {
                case (label, i) => if (label.toLowerCase contains pattern.toLowerCase) true else out.selected(i)
              })
            }).callbackSelectionChanged
          )
          Browser ! "_callbackSelectionChanged" -> (Pipeline.output.selectionAttributes, "left")
          self ! "filter.view" -> Pipeline.input.currentView

          /** Search and select a node depending on it's neighbour label match **/
        case ("selectByNeighbourPattern", pattern: String, category:String) =>
          val ref = Pipeline.input
          val out = Pipeline.output
          println("selectByNeighbourPattern("+pattern+", "+category+")")
          Pipeline.setOutput(
            (if (pattern == null | (pattern.equals(" ") || pattern.isEmpty)) {
              Pipeline.setCategoryCache(Pipeline.categoryCache.clearSelection)
              ref.clearSelection
              out.clearSelection
            } else {
              out + ("selected" -> out.label.zipWithIndex.map {
                case (label, i) =>
                  val originalID = ref.id(out.uuid(i)) // out local graph out has a relative ID (int)..
                                                       // we need to retrieve the reference ID (int) from the UUID (string)
                  var matched = false
                  ref.label.zipWithIndex foreach {
                    case (potentialNeighbourLabel,potentialNeighbourID) =>
                        if (
                         ref.hasAnyLink(potentialNeighbourID,originalID) // if this is a neighbour..
                         && ref.category(potentialNeighbourID).equalsIgnoreCase(category) // that match category..
                         && (potentialNeighbourLabel.toLowerCase contains pattern.toLowerCase) // that match search..
                        ) matched = true    // we select our node
                  }
                  if (matched) true else out.selected(i)
              })
            }).callbackSelectionChanged
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
              if (changed) Pipeline.setOutput(out2.callbackSelectionChanged)

            case 'Click =>
              //println("Click!")
              var somethingIsSelected = false
              val doubleClicked = count match {
                      case 'Simple => false
                      case 'Double => true
                      case any => true
              }
              // TODO a selection counter
              val out2 = out + ("selected" -> out.selected.zipWithIndex.map {
                case (previousSelectionState, i) =>
                  val l = out.size(i) // maths hack
                  val p = out.position(i)
                  val nodeHasBeenTouched = (p.isInRange(o, r) || p.isInRange(o, l + (l / 2.0))) // maths
                  if (nodeHasBeenTouched) somethingIsSelected = true
                  if (nodeHasBeenTouched) println("touched a node of degree "+out.degree(i))
                  (previousSelectionState, nodeHasBeenTouched)
              }.map {
                case (previousSelectionState, nodeHasBeenTouched) =>



                  if (doubleClicked) {
                      nodeHasBeenTouched
                  } else {
                      if (nodeHasBeenTouched) !previousSelectionState else previousSelectionState
                  }
              }.toArray)
              //println("selection count, before: "+out.selection.size+" after: "+out2.selection.size)
              Pipeline.setOutput(out2.callbackSelectionChanged)
              Browser ! "_callbackSelectionChanged" -> (Pipeline.output.selectionAttributes, side match {
                case 'Left => "left"
                case 'Right => "right"
                case any => "none"
              })

              // check if we need to recompute the meso field
              if (doubleClicked) {
                if (somethingIsSelected) {
                    Server ! "filter.view" -> "meso"

                  } else {
                    // zoom?
                  }
              } else {
                  //println("Workflow: updating view (is it OK?)")
                  //self ! "filter.view" -> out2.currentView
              }


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


        case ("export","GEXF") => (new GEXF) ! Pipeline.output
        case x:scala.xml.Elem =>
          Browser ! 'forceDownload -> x.toString
         //  new ExportGraphDialog(x.toString)

        case (key: String, value: Any) =>
           Pipeline.applyKey(key, value)
   
           // WARNING actually caching is not really used (didn't have the time to debug it) so a straightforward
           // workflow is used instead. since it was that easy, I simply resetted the "categoryCache" when we unselect nodes
           // if you happen to refactorate this, you will need to clear the selection in the other caches, too

           // IDEA maybe, a better solution would be to define, for each "modifier", which kind of data is modified,
           // and operation is done
           val out = Pipeline.output
           val output : Graph = null
           //var tmp = new Graph
           //var updateNeeded =
          key match {
            case "filter.view" =>
              println("Workflow: received msg: \""+key+"\"")
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out).updateSelectedWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Filters.category(Pipeline.input))callbackNodeCountChanged)
              Pipeline.setNodeWeightCache(Filters.nodeWeight2(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)).callbackNodeCountChanged)
            case "filter.node.category" =>
              println("Workflow: received msg: \""+key+"\"")
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out).updateSelectedWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Filters.category(Pipeline.input)))
              Pipeline.setNodeWeightCache(Filters.nodeWeight2(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)).callbackNodeAttributesChanged)
            case "filter.a.node.weight" =>
              println("Workflow: received msg: \""+key+"\"")
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out).updateSelectedWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Pipeline.categoryCache.updatePositionWithCategory(out).updateSelectedWithCategory(out)))
              Pipeline.setNodeWeightCache(Filters.nodeWeight2(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)).callbackNodeCountChanged)
            case "filter.a.edge.weight" =>
              println("Workflow: received msg: \""+key+"\"")
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out).updateSelectedWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Pipeline.categoryCache.updatePositionWithCategory(out).updateSelectedWithCategory(out)))
              Pipeline.setNodeWeightCache(Filters.nodeWeight2(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)).callbackEdgeCountChanged)
            case "filter.b.node.weight" =>
              println("Workflow: received msg: \""+key+"\"")
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out).updateSelectedWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Pipeline.categoryCache.updatePositionWithCategory(out).updateSelectedWithCategory(out)))
              Pipeline.setNodeWeightCache(Filters.nodeWeight2(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)).callbackNodeCountChanged)
            case "filter.b.edge.weight" =>
              println("Workflow: received msg: \""+key+"\"")
              Pipeline.setInput(Pipeline.input.updatePositionWithCategory(out).updateSelectedWithCategory(out))
              Pipeline.setCategoryCache(Filters.weightToSize(Pipeline.categoryCache.updatePositionWithCategory(out).updateSelectedWithCategory(out)))
              Pipeline.setNodeWeightCache(Filters.nodeWeight2(Pipeline.categoryCache))
              Pipeline.setEdgeWeightCache(Filters.edgeWeight(Pipeline.nodeWeightCache))
              Pipeline.setOutput(Filters.clean(Filters.category(Pipeline.edgeWeightCache)).callbackEdgeCountChanged)

            case "filter.a.node.size" =>
              println("Workflow: received msg: \""+key+"\"")
              Pipeline.setOutput (
                Pipeline.output.updateSizeWithCategory (
                  Filters.weightToSize (
                    Pipeline.categoryCache.callbackNodeAttributesChanged
                  )
                )
              )

            case "filter.b.node.size" =>
              println("Workflow: received msg: \""+key+"\"")
              Pipeline.setOutput (
                Pipeline.output.updateSizeWithCategory (
                  Filters.weightToSize (
                    Pipeline.categoryCache.callbackNodeAttributesChanged
                  )
                )
              )

            case any => // we don't need to update the scene for other attributes
          }

         /*
         if (pushToOutput) {
              output//.warmCache
         } */
        case s: String =>
        case msg => println("Workflow: unknow msg: " + msg)
      }
    }
  }


}
