/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import org.daizoru._
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
object Server extends node.util.Actor {

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
    "camera.zoom" -> 0.0,
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
          println("exiting server")
          Browser ! 'exit
          Layout ! 'exit
          Workflow ! 'exit
          exit()

        case g: Graph =>
          properties = defaultProperties
          val in = new Graph(properties ++ g.elements)
          properties += "input" -> in
          Pipeline.setInput(in)
          Workflow ! 'graphImported
          Browser ! "_graphImportedCallback" -> "success"

        case x:scala.xml.Elem => Browser ! 'forceDownload -> x.toString

        case ("export","GEXF") =>
          val gx = new GEXF
          gx ! Pipeline.output

        case ('open, pathOrURL: Any) => (new GEXF) ! pathOrURL

        case "recenter"                      => Workflow ! "recenter"
        case ("select", toBeSelected)        => Workflow ! "select"              -> toBeSelected
        case ("selectByPattern", pattern)    => Workflow ! "selectByPattern"     -> pattern
        case ("highlightByPattern", pattern) => Workflow ! "highlightByPattern"  -> pattern

        case ('getNodes,view,category) =>
          println("Server: asekd for 'getNodes "+view+" "+category)
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
