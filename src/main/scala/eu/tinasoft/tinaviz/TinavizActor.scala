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

import actors._
import Actor._

class TinavizActor extends node.util.Actor {
  

  // scene cache
  var scene : Scene = new Scene()

  var properties : Map[String,Any] = Map(
    "profiler.fps" -> 0,
    "scene.pause" -> false,
    "scene.debug" -> true
  )
  start
  
  def act() {


    
    // we could eventually keep a
    //var model = new Model()
    
    while(true) {
      receive {

        case ('updateNode,value) =>
          //context ! 'updateNode -> value

          // scene builder update the scene!
        case newScene:Scene => scene = newScene

          // main want the scene!
        case 'getScene => reply(scene)


        case ('updated,key:String,value:Any,previous:Any) =>
          // log("ignoring update of "+key)
              

        case ('openURL,url:String) =>
          val buf = new StringBuilder
          io.Source.fromURL(new java.net.URL(url)).foreach(buf.append)
          self ! 'openString -> buf.toString
          
        case ('openString,str:String) => (new GEXFImporter) ! str
                 
        case 'updateScene =>
          
         
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
          reply(previous)
          if (!previous.equals(value)) {
            self ! ('updated,key,value,previous)
          }

        case msg => println("unknow msg: "+msg)
      }
    }
    
  }
  
  /**
   * The most important function, that does everything
   */
  def buildScene() = {
    val view = properties("scene.view") 
    var s = new MutableScene()
    view match {
      case "macro" =>
        
      case "meso" =>
        // construct a subview of macro
        val view = properties("scene.view")
    }
    self ! s.toScene
  }
  
}
