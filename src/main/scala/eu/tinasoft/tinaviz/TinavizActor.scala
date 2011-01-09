/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import org.daizoru._
import eu.tinasoft._

import tinaviz.data._
import tinaviz.graph._
import tinaviz.context._
import Sketch._

import actors._
import Actor._

class TinavizActor extends node.util.Actor {

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

    // camera
    "zoom" -> 0.0,
    "position" -> (0.0,0.0),

    //  workflow
    "pipeline" -> List ("viewFilter", "nodeWeightFilter", "edgeWeightFilter"),

    // final scene
    "scene" -> new Scene()
  )

  var properties : Map[String,Any] = defaultProperties

  // internal states: 'needUpdate 'updating  'upToDate
  //var state = 'upToDate

  // pipeline data
  var pipeline : Map[String,(Graph,Sketch)] = Map(
    "global" -> (new Graph(),new Sketch()),
    "viewFilter" -> (new Graph(),new Sketch()),
    "nodeWeightFilter" -> (new Graph(),new Sketch()),
    "edgeWeightFilter" -> (new Graph(),new Sketch())
  )

  start

  def act() {
   
    while(true) {
      receive {

        case valeur:Float =>
          reply(valeur * 2)

        case ('updateNode,value) =>
          //context ! 'updateNode -> value

          // receive a new graph
        case graph:Graph =>
          properties = defaultProperties
          pipeline = pipeline.map { case (k,v) => ( k,(new Graph(),new Sketch()) ) }
          val sketch = graph:Sketch
          pipeline += "global" -> (graph,sketch)
          self ! "scene" -> (sketch:Scene)
          
        case ('updated,key:String,value:Any,previous:Any) =>
          // log("ignoring update of "+key)
              

        case ('openString,str:String) => (new GEXFImporter) ! str
        case ('openURL,url:String) =>
          val buf = new StringBuilder
          io.Source.fromURL(new java.net.URL(url)).foreach(buf.append)
          self ! 'openString -> buf.toString


        case key:String =>
          //if (properties.contains(k))
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

        case msg => println("TinavizActor: unknow msg: "+msg)
      }
    }
    
  }
  
  def get[T](key:String) : T = properties.get(key).get.asInstanceOf[T]
}
