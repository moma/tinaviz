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

  val sketcher = new Sketcher()
  val spatializer  = new Spatializer()


  var pipeline = List('category,'layout)
  var pipelineData = List(new Graph())
  var sync = true

    
  start

  def act() {
    
    // internal states: 'needUpdate 'updating  'upToDate
    //var state = 'upToDate


    while(true) {
      receive {

        case ('updateNode,value) =>
          //context ! 'updateNode -> value

          // receive a brand new graph
        case graph:Graph => 
          //if (sender.receiver == sketcher) {
          println("PipelineOriginal "+graph.nbNodes+" nodes, "+graph.nbEdges+" edges.")
          properties = defaultProperties
          pipelineData = List(graph)
          pipeline = List('category,'layout)
          sync = false

        case (p:List[Symbol],graph) =>

          if (point == pipeline.size) {
            sketcher ! graph
          }

            


        case ("frameRate", value:Any) =>
          if (doPipeline) {
            doPipeline = false
            pipeline ! output
          }

        case scene:Scene =>
          //println("Tinaviz: sending to screen..")
          properties += "scene" -> scene


        case ('updated, 'nodeWeight, value:Any, previous:Any) =>
          
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
