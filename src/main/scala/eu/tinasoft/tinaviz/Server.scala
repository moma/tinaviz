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

package eu.tinasoft.tinaviz

import eu.tinasoft._

import tinaviz.io._
import tinaviz.graph._
import tinaviz.scene._
import tinaviz.pipeline._
import tinaviz.layout._

import actors._
import Actor._

case class Step(val step: Symbol)

/**
 * This class need a big refactoring..
 */
object Server extends Actor {

  val defaultProperties: Map[String, Any] = Map(
    // global real FPS
    "frameRate" -> 0,
    "filter.selection" -> List.empty[String],

    // current view settings
    "filter.view" -> "macro",
    "filter.node.category" -> "Document",
    "filter.a.node.weight" -> (0.0, 1.0),
    "filter.a.edge.weight" -> (0.0, 1.0),
    "filter.a.node.size" -> .2,
    "filter.b.node.weight" -> (0.0, 1.0),
    "filter.b.edge.weight" -> (0.0, 1.0),
    "filter.b.node.size" -> .2,
    "layout.gravity" -> 1.3, // stronger means faster!
    "layout.attraction" -> 1.01,
    "layout.repulsion" -> 1.5,
    "pause" -> false,
    "debug" -> false,

    // global selection disk settings
    "selectionRadius" -> 10.0,

    // TODO real-time camera settings
    "camera.zoom" -> 0.5,
    "camera.position" -> (0.0, 0.0),
    "camera.target" -> "all",

    "views.macro.pause" -> false,
    "views.macro.debug" -> false,

    "screen.width" -> 100,
    "screen.height" -> 100,

    "input" -> new Graph()
  )

  var properties: Map[String, Any] = defaultProperties

  def act() {

    while (true) {
      receive {
        case 'exit =>
          println("Server: calling exit() on myself")
          exit()

        case g: Graph =>
          properties = defaultProperties
          val in = new Graph(properties ++ g.elements).callbackGraphChanged // brand new graph!  maybe the callback is too much

          properties += "input" -> in
          Pipeline.setInput(in)
          Workflow ! 'graphImported
          Browser ! "_graphImportedCallback" -> "success"

        case ("export","GEXF") =>  Workflow ! ("export","GEXF")
        case ('open, pathOrURL: Any) =>  (new GEXF) ! (pathOrURL, properties)

        case ("select", toBeSelected)        => Workflow ! "select"              -> toBeSelected
        case ("selectByPattern", pattern)    => Workflow ! "selectByPattern"     -> pattern
        case ("selectByNeighbourPattern", pattern, category)    => Workflow ! ("selectByNeighbourPattern", pattern, category)
        case ("highlightByPattern", pattern) => Workflow ! "highlightByPattern"  -> pattern

        case ('getNodes,view,category) =>
          println("Server: client called getNodes("+view+", "+category+") -> replying..")
          reply(Workflow !? ('getNodes,view,category))

        case ('getNeighbourhood,view,todoList) =>
          Workflow ! ('getNeighbourhood,view,todoList)

        case ('getNodeAttributes,uuid) =>
          reply (Workflow !? 'getNodesAttributes -> uuid)

        case ("camera.mouse", kind, side, count, position) =>
          Workflow ! ("camera.mouse", kind, side, count, position)

        case ('updated, key: String, value: Any, previous: Any) =>
          key match {
            case "filter.view" =>
              Browser ! "_callbackViewChanged" -> value
              Workflow ! key -> value
            //case "camera.zoom" =>
            //case "camera.position" =>
            case "frameRate" =>
            case any => Workflow ! key -> value
          }
        case key: String =>  reply(properties(key))

        case (key: String, pvalue: Any) =>
          var value = pvalue
          if (!properties.contains(key)) {
            properties += key -> value
            self ! ('updated, key, value, value)
          } else {
            val previous = properties(key)
            value = value match {
            // special case for booleans
              case 'toggle =>
                previous match {
                  case b: Boolean =>
                    !b
                  case s: String =>
                    s match {
                      case "Document" => "NGram"
                      case "NGram" => "Document"
                      case "macro" => "meso"
                      case "meso" => "macro"
                      case any => any
                    }
                  case x => x
                }
              // default case
              case x => value
            }
            properties += key -> value
            //
            //reply(previous)
            if (!previous.equals(value)) {
              self ! ('updated, key, value, previous)
            }
          }

        case msg => println("Tinaviz: error, unknow msg: " + msg)
      }
    }

  }

  def get[T](key: String): T = properties.get(key).get.asInstanceOf[T]
}
