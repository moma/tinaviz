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

    //  workflow
    //"Pipeline" -> List ("viewFilter", "nodeWeightFilter", "edgeWeightFilter"),

    "screen.width" -> 100,
    "screen.height" -> 100,
    // final scene
    "input" -> new Graph(),
    "output" -> new Graph()
  )

  var properties: Map[String, Any] = defaultProperties

  start


  def act() {

    // internal states: 'needUpdate 'updating  'upToDate
    //var state = 'upToDate

    var pauseBuffer = false

    while (true) {
      receive {

        case ('updateNode, value) =>
        //context ! 'updateNode -> value

        // receive a brand new graph
        case g: Graph =>
        //if (sender.receiver == sketcher) {
          properties = defaultProperties
          val in = new Graph(properties ++ g.elements)
          properties += "input" -> in
          Pipeline ! in
          Browser ! "_graphImportedCallback" -> "success"
        //PipelineBusy = false


        case x:scala.xml.Elem =>
          //println("Got XML: "+x)
          Browser ! 'forceDownload -> x.toString
          
        // import/export functions
        case ("export","gexf") => 
          (new GEXF) ! properties("output")
        case ('open, pathOrURL: Any) => (new GEXF) ! pathOrURL

        case ('output, graph:Graph) =>
          properties += "output" -> graph

        case "recenter" =>
          Pipeline ! "recenter"

        case ("select", toBeSelected) =>
          Pipeline ! "select" -> toBeSelected
          
        case("selectByPattern",pattern) =>
          Pipeline ! "selectByPattern" -> pattern
          
        case("highlightByPattern",pattern) =>
          Pipeline ! "highlightByPattern" -> pattern

        case ('getNodes,view,category) =>
          println("Server: asekd for 'getNodes "+view+" "+category)
          reply(Pipeline !? ('getNodes,view,category))

        case ('getNeighbourhood,view,todoList) =>
          Pipeline ! ('getNeighbourhood,view,todoList)

        case ('getNodeAttributes,uuid) =>
          reply (Pipeline !? 'getNodesAttributes -> uuid)

        // TODO do something for this, it looks a bit creepy
        case ("filter.a.node.weight.min", value: Double) =>
          self ! (("filter.a.node.weight", (value, properties("filter.a.node.weight").asInstanceOf[(Double, Double)]._2)))

        case ("filter.a.node.weight.max", value: Double) =>
          self ! (("filter.a.node.weight", (properties("filter.a.node.weight").asInstanceOf[(Double, Double)]._1, value)))

        case ("filter.a.edge.weight.min", value: Double) =>
          self ! (("filter.a.edge.weight", (value, properties("filter.a.edge.weight").asInstanceOf[(Double, Double)]._2)))

        case ("filter.a.edge.weight.max", value: Double) =>
          self ! (("filter.a.edge.weight", (properties("filter.a.edge.weight").asInstanceOf[(Double, Double)]._1, value)))

        case "filter.a.node.weight.min" =>
          reply(properties("filter.a.node.weight").asInstanceOf[(Double, Double)]._1)

        case "filter.a.node.weight.max" =>
          reply(properties("filter.a.node.weight").asInstanceOf[(Double, Double)]._2)

        case "filter.a.edge.weight.min" =>
          reply(properties("filter.a.edge.weight").asInstanceOf[(Double, Double)]._1)

        case "filter.a.edge.weight.max" =>
          reply(properties("filter.a.edge.weight").asInstanceOf[(Double, Double)]._2)

        // TODO do something for this, it looks a bit creepy
        case ("filter.b.node.weight.min", value: Double) =>
          self ! (("filter.b.node.weight", (value, properties("filter.b.node.weight").asInstanceOf[(Double, Double)]._2)))

        case ("filter.b.node.weight.max", value: Double) =>
          self ! (("filter.b.node.weight", (properties("filter.b.node.weight").asInstanceOf[(Double, Double)]._1, value)))

        case ("filter.b.edge.weight.min", value: Double) =>
          self ! (("filter.b.edge.weight", (value, properties("filter.b.edge.weight").asInstanceOf[(Double, Double)]._2)))

        case ("filter.b.edge.weight.max", value: Double) =>
          self ! (("filter.b.edge.weight", (properties("filter.b.edge.weight").asInstanceOf[(Double, Double)]._1, value)))

        case "filter.b.node.weight.min" =>
          reply(properties("filter.b.node.weight").asInstanceOf[(Double, Double)]._1)

        case "filter.b.node.weight.max" =>
          reply(properties("filter.b.node.weight").asInstanceOf[(Double, Double)]._2)

        case "filter.b.edge.weight.min" =>
          reply(properties("filter.b.edge.weight").asInstanceOf[(Double, Double)]._1)

        case "filter.b.edge.weight.max" =>
          reply(properties("filter.b.edge.weight").asInstanceOf[(Double, Double)]._2)

        case ("camera.mouse", kind, side, count, position) =>
          Pipeline ! ("camera.mouse", kind, side, count, position)

        case ('updated, key: String, value: Any, previous: Any) =>
        // log("ignoring update of "+key)
        //println("updating Pipeline with this data: "+key+" -> "+value)
          key match {
            //case "camera.zoom" =>
            //case "camera.position" =>
            case "frameRate" =>
            case any => Pipeline ! key -> value // update the Pipeline
          }



        case key: String =>
          reply(properties(key))


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
