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
import Sketch._

import actors._
import Actor._

case class Step(val step:Symbol)

class Server extends node.util.Actor {

  val defaultProperties : Map[String,Any] = Map(
    // global real FPS
    "frameRate" -> 0,

    // current view settings
    "view" -> "macro",
    "category" -> "NGram",
    "pause" -> false,
    "debug" -> true,

    // global selection disk settings
    "selectionRadius" -> 10.0,

    // TODO real-time camera settings
    "zoom" -> 0.0,
    "position" -> (0.0,0.0),

    //  workflow
    //"pipeline" -> List ("viewFilter", "nodeWeightFilter", "edgeWeightFilter"),

    // final scene
    "scene" -> new Scene()
  )

  var properties : Map[String,Any] = defaultProperties


  start

  def act() {
    
    // internal states: 'needUpdate 'updating  'upToDate
    //var state = 'upToDate

    // pipeline data
    var input = new Graph()
    var output = new Graph()

    val sketcher = new Sketcher()
    val spatializer  = new Spatializer()

    while(true) {
      receive {

        case ('updateNode,value) =>
          //context ! 'updateNode -> value

          // receive a brand new graph
        case graph:Graph =>
          //if (sender.receiver == sketcher) {
            println("Tinaviz: loaded "+graph.nbNodes+" nodes, "+graph.nbEdges+" edges.")
            properties = defaultProperties
            input = graph
            //self ! 'process
            //spatializer ! graph
            
          //} else if (sender.receiver  == spatializer) {
          //}

        case ('spatialized,graph:Graph) =>
          println("received spatialized Graph, sending to sketcher")
         sketcher ! input
          
        case 'process =>
          println("Tinaviz: doing some thread-blocking processing..")
          //output = input
          //sketcher ! output

        case scene:Scene =>
          println("Tinaviz: sending to screen..")
          properties += "scene" -> scene
          
          
        case ('updated, "frameRate", value:Any,previous:Any) =>
          println("received an update for frameRate")
          spatializer ! output
            
        case ('updated,key:String,value:Any,previous:Any) =>
          // log("ignoring update of "+key)
              

        case ('open, any:Any) => (new GEXF) ! any

        case key:String =>
          reply(properties(key))
        
        case (key:String,value:Any) =>
          //if (properties.contains(k)) {
          val previous = properties(key)
          value match {
            // special case for booleans
            case 'toggle =>
              previous match {
                case b:Boolean =>
                  properties += key -> !b
                case x =>
                  throw new Exception("")
              }
              // default case
            case x => properties += key -> value
          }
          //reply(previous)
          if (!previous.equals(value)) {
            self ! ('updated,key,value,previous)
          }

        case msg => println("Tinaviz: error, unknow msg: "+msg)
      }
    }
    
  }
  
  def get[T](key:String) : T = properties.get(key).get.asInstanceOf[T]
}
