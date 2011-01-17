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
    "filter.selection" -> List.empty[String],

    // current view settings
    "filter.view" -> "macro",
    "filter.node.category" -> "Document",
    "filter.node.weight.min" -> 1.0,
    "filter.node.weight.max" -> 0.0,
    "filter.edge.weight.min" -> 1.0,
    "filter.edge.weight.max" -> 0.0,
    "layout.gravity" ->  1.2, // stronger means faster!
    "layout.attraction" -> 10.0,
    "layout.repulsion" -> -1.4,
    "pause" -> false,
    "debug" -> true,

    // global selection disk settings
    "selectionRadius" -> 10.0,

    // TODO real-time camera settings
    "camera.zoom" -> 0.0,
    "camera.position" -> (0.0,0.0),

    "views.macro.pause" -> false,
    "views.macro.debug" -> false,

    //  workflow
    //"pipeline" -> List ("viewFilter", "nodeWeightFilter", "edgeWeightFilter"),

    // final scene
    "scene" -> new Scene()
  )

  var properties : Map[String,Any] = defaultProperties

  val pipeline  = new Pipeline(this)
  var pipelineBusy = true

  var graph = new Graph() // used for some stats

  start

  
  def act() {
    
    // internal states: 'needUpdate 'updating  'upToDate
    //var state = 'upToDate


    while(true) {
      receive {

        case ('updateNode,value) =>
          //context ! 'updateNode -> value

          // receive a brand new graph
        case g:Graph => 
          //if (sender.receiver == sketcher) {
          properties = defaultProperties
          graph = g
          pipeline ! g
          pipelineBusy = false

        case scene:Scene =>
          properties += "scene" -> scene
          pipelineBusy = false
          self ! "frameRate" -> properties("frameRate") // force relaunching

        case ("recenter",true) =>
            println("TODO recentering")
            //properties += "recenter" -> false

        case ("select",uuid:String) =>
             pipeline ! "select" -> uuid
             
        case ('updated,"frameRate",value:Any,previous:Any) =>

          val pause = get[Boolean]("pause") //: Boolean = try { get[Boolean]("pause") } catch { case e => true }
          if (!pause) {
            if (!pipelineBusy) {
              pipelineBusy = true
              pipeline ! "frameRate" -> value
            } else {
              //println("could not update pipeline, too busy..")
            }
          }

        case ('updated,key:String,value:Any,previous:Any) =>
          // log("ignoring update of "+key)
          //println("updating pipeline with this data: "+key+" -> "+value)
          pipeline ! key -> value // update the pipeline

        case ('open, any:Any) => (new GEXF) ! any
          // cb ! true

        case key:String =>
          reply(properties(key))


        case (key:String,value:Any) =>
          if (!properties.contains(key)) {
            properties += key -> value
            self ! ('updated,key,value,value)
          } else {
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

            //
            //reply(previous)
            if (!previous.equals(value)) {
              self ! ('updated,key,value,previous)
            }
          }

        case msg => println("Tinaviz: error, unknow msg: "+msg)
      }
    }
    
  }
  
  def get[T](key:String) : T = properties.get(key).get.asInstanceOf[T]
}
