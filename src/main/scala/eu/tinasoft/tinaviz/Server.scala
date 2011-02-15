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
import tinaviz.sketch._
import tinaviz.pipeline._
import Sketch._

import actors._
import Actor._

case class Step(val step: Symbol)

class Server extends node.util.Actor {

  val defaultProperties: Map[String, Any] = Map(
    // global real FPS
    "frameRate" -> 0,
    "filter.selection" -> List.empty[String],

    // current view settings
    "filter.view" -> "macro",
    "filter.node.category" -> "Document",
    "filter.node.weight" -> (0.0, 1.0),
    "filter.edge.weight" -> (0.0, 1.0),
    "filter.node.size" -> .2,
    "layout.gravity" -> 1.1, // stronger means faster!
    "layout.attraction" -> 1.01,
    "layout.repulsion" -> 1.5,
    "pause" -> false,
    "debug" -> true,

    // global selection disk settings
    "selectionRadius" -> 10.0,

    // TODO real-time camera settings
    "camera.zoom" -> 0.0,
    "camera.position" -> (0.0, 0.0),

    "views.macro.pause" -> false,
    "views.macro.debug" -> false,

    //  workflow
    //"pipeline" -> List ("viewFilter", "nodeWeightFilter", "edgeWeightFilter"),

    // final scene
    "scene" -> new Scene()
  )

  var properties: Map[String, Any] = defaultProperties

  val pipeline = new Pipeline(this)
  var pipelineBusy = true

  var input = new Graph()
  var output = new Graph()
  // used for some stats

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
          input = new Graph(properties ++ g.elements)
          pipeline ! input
        //pipelineBusy = false

        case (graph: Graph, scene: Scene) =>
          properties += "scene" -> scene
          output = graph
        //pipelineBusy = false
        //self ! "frameRate" -> properties("frameRate") // force relaunching

        case "recenter" =>
          pipeline ! "recenter"

        case ("select", uuid) =>
          pipeline ! "select" -> uuid

        case ('getNodes,view,category) =>
          println("Server: asekd for 'getNodes "+view+" "+category)
          reply(pipeline !? ('getNodes,view,category))

        case ('getNeighbourhood,view,todoList) =>
          reply(pipeline !? ('getNeighbourhood,view,todoList))

        case ('getNodeAttributes,uuid:String) =>
          reply (pipeline !? 'getNodesAttributes -> uuid)

        // TODO do something for this, it looks a bit creepy
        case ("filter.node.weight.min", value: Double) =>
          self ! (("filter.node.weight", (value, properties("filter.node.weight").asInstanceOf[(Double, Double)]._2)))

        case ("filter.node.weight.max", value: Double) =>
          self ! (("filter.node.weight", (properties("filter.node.weight").asInstanceOf[(Double, Double)]._1, value)))

        case ("filter.edge.weight.min", value: Double) =>
          self ! (("filter.edge.weight", (value, properties("filter.edge.weight").asInstanceOf[(Double, Double)]._2)))

        case ("filter.edge.weight.max", value: Double) =>
          self ! (("filter.edge.weight", (properties("filter.edge.weight").asInstanceOf[(Double, Double)]._1, value)))

        case "filter.node.weight.min" =>
          reply(properties("filter.node.weight").asInstanceOf[(Double, Double)]._1)

        case "filter.node.weight.max" =>
          reply(properties("filter.node.weight").asInstanceOf[(Double, Double)]._2)

        case "filter.edge.weight.min" =>
          reply(properties("filter.edge.weight").asInstanceOf[(Double, Double)]._1)

        case "filter.edge.weight.max" =>
          reply(properties("filter.edge.weight").asInstanceOf[(Double, Double)]._2)

        case ("camera.mouse", kind, side, count, position) =>
          pipeline ! ("camera.mouse", kind, side, count, position)

        case ('updated, key: String, value: Any, previous: Any) =>
        // log("ignoring update of "+key)
        //println("updating pipeline with this data: "+key+" -> "+value)
          key match {
            //case "camera.zoom" =>
            //case "camera.position" =>
            case "frameRate" =>
            case any => pipeline ! key -> value // update the pipeline
          }


        case ('open, any: Any) => (new GEXF) ! any
        // cb ! true

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
