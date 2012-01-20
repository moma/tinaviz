/************************************************************************
                                  Tinaviz
 * ************************************************************************
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
import tinaviz.io.json.Json
import tinaviz.io.Webpage
import tinaviz.io.GEXF
import tinaviz.util.Vector._
import tinaviz.util.Maths
import tinaviz.layout._

import actors.Actor
import actors.Actor._

import compat.Platform
import com.lowagie.text.pdf.codec.Base64.OutputStream
import tinaviz.{Session, Main, Server}
import reflect.ValDef

/**
 *
 */
class Workflow(val session: Session) extends Actor {

  def act() {

    val pipeline = session.pipeline

    while (true) {
      receive {

        case 'exit =>
          println("Workflow: exiting")
          exit()

        /*
         'overwriteCoordinates -> Graph

         The Workflow actor is the only one who can updated the pipeline's graph layout
         What we are doing here is very simple:
         We updated the output (visualized) graph with new coordinates, then we regenerate all the attributes
         from the new data. we also warm the "lazy val" cache.
         Computing lazy vals can take some time, so if we were to do it in the main thread (draw())
         it would make it slower and freezy.
         We are doing it here in the Workflow actor, so it don't block the rendering.
         */
        case ('overwriteCoordinates, g: Graph) =>
          pipeline.setInput(pipeline.input.updatePositionWithCategory(g))
          pipeline.setOutput(pipeline.output.updatePositionWithCategory(g).callbackPositionsChanged.warmCache)
          session.layout ! 'run

        case (cb: Int, something) =>
          session.webpage ! cb -> (something match {

            case ("pause", true) =>
              //println("Workflow: pause -> stopping layout")
              session.layout ! 'stop

            case ("false", true) =>
              //println("Workflow: pause disabled -> starting layout")
              session.layout ! 'start

            case ('getNodeAttributes, uuid: String) =>
              //println("Workflow: asked for 'getNodeAttributes (on INPUT GRAPH) of " + uuid)
              Map("nodes" -> pipeline.input.lessAttributes(uuid))

            case ('getNeighbourhood, view: String, todoList: List[String]) =>
              val in = pipeline.input
              val out = pipeline.output
              val container = (view match {
                case "meso" => out
                case any => in
              })
              val neighbourList = Map(todoList.zipWithIndex: _*).map {
                case (uuid, i) => (uuid, container.neighbours(container.id(uuid)))
              }
              val nodeList = out.selectionUUID.toList

              //System.out.println("calling callback with this data: " + (nodeList, neighbourList))
              Map("nodes" -> nodeList, "neighbours" -> neighbourList)

            case ('getNeighbourhood, view: String, "selection") =>
              val in = pipeline.input
              val out = pipeline.output
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
              Map("nodes" -> nodeList, "neighbours" -> neighbourList)


            case ('getNodes, view: String, category: String) =>
              //println("Server: asked for 'getNodes " + view + " " + category)
              val in = pipeline.input
              val out = pipeline.output
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
              Map("nodes" -> (result.map {
                case (k, v) => v
              }.toList))

            case ("select", uuidList: List[String]) =>
              // println("selecting nodes: '" + uuidList + "'")
              val in = pipeline.input
              val out = pipeline.output
              val out2 = if (uuidList.size == 0) {
                // pipeline.setCategoryCache(pipeline.categoryCache.clearSelection)
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
              pipeline.setOutput(out2.callbackSelectionChanged)
              self ! "filter.view" -> in.currentView
              Map("selection" -> pipeline.output.selectionAttributes, "mouse" -> "left")


            case ("select", uuid: String) =>
              val in = pipeline.input
              val out = pipeline.output
              val out2 = if (uuid == null | (uuid.equals(" ") || uuid.isEmpty)) {
                //pipeline.setCategoryCache(pipeline.categoryCache.clearSelection)
                in.clearSelection
                out.clearSelection
              } else {
                out + ("selected" -> out.uuid.zipWithIndex.map {
                  case (_uuid, i) =>
                    val res = if (_uuid equals uuid) true else out.selected(i)
                    res
                })
              }
              pipeline.setOutput(out2.callbackSelectionChanged)
              //println("calling pipeline.output.updateSelectedWithCategory( g )")
              /*pipeline.output.updateSelectedWithCategory(
                 out2
             ) */
              //println("out2.selection.size: " + out2.selection.size)
              // println("pipeline.output.size: " + pipeline.output.size)
              //session.webpage ! cb -> (pipeline.output.selectionAttributes, "left")
              Map("selection" -> pipeline.output.selectionAttributes, "mouse" -> "left")
            //self ! "filter.view" -> pipeline.input.currentView


            case ("selectByPattern", pattern: String) =>
              val in = pipeline.input
              val out = pipeline.output
              pipeline.setOutput(
                (if (pattern == null | (pattern.equals(" ") || pattern.isEmpty)) {
                  // pipeline.setCategoryCache(pipeline.categoryCache.clearSelection)
                  in.clearSelection
                  out.clearSelection
                } else {
                  out + ("selected" -> out.label.zipWithIndex.map {
                    case (label, i) => if (label.toLowerCase contains pattern.toLowerCase) true else out.selected(i)
                  })
                }).callbackSelectionChanged
              )
              self ! "filter.view" -> pipeline.input.currentView
              Map("selection" -> pipeline.output.selectionAttributes, "mouse" -> "left")

            /**Search and select a node depending on it's neighbour label match **/
            case ("selectByNeighbourPattern", pattern: String, category: String) =>
              val ref = pipeline.input
              val out = pipeline.output
              println("selectByNeighbourPattern(" + pattern + ", " + category + ")")
              pipeline.setOutput(
                (if (pattern == null | (pattern.equals(" ") || pattern.isEmpty)) {
                  //pipeline.setCategoryCache(pipeline.categoryCache.clearSelection)
                  ref.clearSelection
                  out.clearSelection
                } else {
                  out + ("selected" -> out.label.zipWithIndex.map {
                    case (label, i) =>
                      val originalID = ref.id(out.uuid(i)) // out local graph out has a relative ID (int)..
                      // we need to retrieve the reference ID (int) from the UUID (string)
                      var matched = false
                      ref.label.zipWithIndex foreach {
                        case (potentialNeighbourLabel, potentialNeighbourID) =>
                          if (
                            ref.hasAnyLink(potentialNeighbourID, originalID) // if this is a neighbour..
                              && ref.category(potentialNeighbourID).equalsIgnoreCase(category) // that match category..
                              && (potentialNeighbourLabel.toLowerCase contains pattern.toLowerCase) // that match search..
                          ) matched = true // we select our node
                      }
                      if (matched) true else out.selected(i)
                  })
                }).callbackSelectionChanged
              )
              self ! "filter.view" -> pipeline.input.currentView
              Map("selection" -> pipeline.output.selectionAttributes, "mouse" -> "left")


            case ("highlightByPattern", pattern: String) =>
              val in = pipeline.input
              val out = pipeline.output
              pipeline.setOutput(out + ("highlighted" -> out.label.map {
                case label => if (pattern == null | pattern.isEmpty) false else (label.toLowerCase contains pattern.toLowerCase)
              }))
              //Webpage ! "_callbackSelectionChanged" -> "left"
              self ! "filter.view" -> in.currentView // will automatically update the highlight section
              Map()

            case (key: String, value) =>

              println("Workflow: " + cb + " -> " + key + " -> " + value)


              pipeline.applyKey(key, value)

              // WARNING actually caching is not really used (didn't have the time to debug it) so a straightforward
              // workflow is used instead. since it was that easy, I simply resetted the "categoryCache" when we unselect nodes
              // if you happen to refactorate this, you will need to clear the selection in the other caches, too

              // IDEA maybe, a better solution would be to define, for each "modifier", which kind of data is modified,
              // and operation is done

              val out = pipeline.output
              val f = pipeline.input.updateSelectedWithCategory(out)
              val output: Graph = null
              //var tmp = new Graph
              //var updateNeeded =
              key match {
                case "filter.view" =>
                  println("Workflow: received msg: \"" + key + "\"")
                  val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
                  val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
                  pipeline.setOutput(h)
                case "filter.node.category" => // might impact the filters!
                  println("Workflow: received msg: \"" + key + "\"")
                  var ff = Filters.nodeWeight2(f, f)
                  ff = Filters.edgeWeight(ff, f)
                  //println("fff: "+ff.uuid.size)
                  val g = Filters.weightToSize(ff, f)
                  //println("g: "+g.uuid.size)
                  //println("g':"+Filters.clean(Filters.category(g)).uuid.size)
                  val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
                  //println("h: "+h.uuid.size)
                  pipeline.setOutput(h)

                case "filter.a.node.weight" =>
                  println("Workflow: received msg: \"" + key + "\"")
                  val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
                  val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
                  pipeline.setOutput(h)
                case "filter.a.edge.weight" =>
                  val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
                  val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
                  pipeline.setOutput(h)
                case "filter.b.node.weight" =>
                  println("Workflow: received msg: \"" + key + "\"")
                  val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
                  val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
                  pipeline.setOutput(h)
                case "filter.b.edge.weight" =>
                  println("Workflow: received msg: \"" + key + "\"")
                  val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
                  val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
                  pipeline.setOutput(h)

                case "filter.a.node.size" =>
                  println("Workflow: received msg: \"" + key + "\"")
                  val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
                  val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
                  pipeline.setOutput(h)

                case "filter.b.node.size" =>
                  println("Workflow: received msg: \"" + key + "\"")
                  val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
                  val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
                  pipeline.setOutput(h)

                case any => // we don't need to update the scene for other attributes

              }
              Map(key -> value)

            case any =>
              println("unknow cb -> msg "+any)
              Map()
          })

        case ("camera.mouse", kind: Symbol, side: Symbol, count: Symbol, position: (Double, Double)) =>
          val out = pipeline.output
          val (cz, cp, sr) = (out.cameraZoom, out.cameraPosition, out.selectionRadius)
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
              if (changed) pipeline.setOutput(out2.callbackSelectionChanged)

            case ('Click) =>
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
                  if (nodeHasBeenTouched) println("touched a node of degree " + out.degree(i))
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
              pipeline.setOutput(out2.callbackSelectionChanged)
              session.webpage ! session.webpage.CB_CLICK -> Map(
                "selection" -> pipeline.output.selectionAttributes,
                "mouse" -> (side match {
                  case 'Left => if (doubleClicked) "doubleLeft" else "left"
                  case 'Right => "right"
                  case any => "none"
                })
              )

              // check if we need to recompute the meso field
              if (doubleClicked) {
                if (somethingIsSelected) {
                  //out2.currentView match {
                  // case "macro" =>
                  // obsolete (will give "error, filter has no "view" method" error)
                  //session.webpage ! "filter.view" -> "meso"
                  //}

                } else {
                  // zoom?
                }
              } else {
                //println("Workflow: updating view (is it OK?)")
                //self ! "filter.view" -> out2.currentView
              }


            case 'Drag =>
              val pause = try {
                pipeline.input.pause
              } catch {
                case x => true
              }
            //self ! "pause" -> true
            case 'Release =>
            //pauseBugger = false
            //self ! "pause" -> pauseBuffer
            case any =>
          }


        case ("export", "GEXF") => (new GEXF(session)) ! pipeline.output
        case x: scala.xml.Elem =>
          session.webpage ! 'download -> Map("gexf" -> x.toString)
        //  new ExportGraphDialog(x.toString)

       // case (key: String, value: Any) =>
       //   setSome(-1, key, value)
        //pipeline.applyKey(key, value)

        /*
       if (pushToOutput) {
            output//.warmCache
       } */

        case (key: String, value) =>

          println("Workflow: " + key + " -> " + value)


          pipeline.applyKey(key, value)

          // WARNING actually caching is not really used (didn't have the time to debug it) so a straightforward
          // workflow is used instead. since it was that easy, I simply resetted the "categoryCache" when we unselect nodes
          // if you happen to refactorate this, you will need to clear the selection in the other caches, too

          // IDEA maybe, a better solution would be to define, for each "modifier", which kind of data is modified,
          // and operation is done

          val out = pipeline.output
          val f = pipeline.input.updateSelectedWithCategory(out)
          val output: Graph = null
          //var tmp = new Graph
          //var updateNeeded =
          key match {
            case "filter.view" =>
              println("Workflow: received msg: \"" + key + "\"")
              val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
              val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
              pipeline.setOutput(h)
            case "filter.node.category" => // might impact the filters!
              println("Workflow: received msg: \"" + key + "\"")
              var ff = Filters.nodeWeight2(f, f)
              ff = Filters.edgeWeight(ff, f)
              //println("fff: "+ff.uuid.size)
              val g = Filters.weightToSize(ff, f)
              //println("g: "+g.uuid.size)
              //println("g':"+Filters.clean(Filters.category(g)).uuid.size)
              val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
              //println("h: "+h.uuid.size)
              pipeline.setOutput(h)

            case "filter.a.node.weight" =>
              println("Workflow: received msg: \"" + key + "\"")
              val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
              val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
              pipeline.setOutput(h)
            case "filter.a.edge.weight" =>
              val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
              val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
              pipeline.setOutput(h)
            case "filter.b.node.weight" =>
              println("Workflow: received msg: \"" + key + "\"")
              val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
              val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
              pipeline.setOutput(h)
            case "filter.b.edge.weight" =>
              println("Workflow: received msg: \"" + key + "\"")
              val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
              val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
              pipeline.setOutput(h)

            case "filter.a.node.size" =>
              println("Workflow: received msg: \"" + key + "\"")
              val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
              val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
              pipeline.setOutput(h)

            case "filter.b.node.size" =>
              println("Workflow: received msg: \"" + key + "\"")
              val g = Filters.weightToSize(Filters.edgeWeight(Filters.nodeWeight2(f, f), f), f)
              val h = Filters.clean(Filters.category(g)).callbackNodeCountChanged
              pipeline.setOutput(h)

            case any => // we don't need to update the scene for other attributes

          }

        case s: String =>

        case msg => println("Workflow: unknow single msg: " + msg)
      }
    }
  }


}
