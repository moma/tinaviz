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
  

  var properties : Map[String,Any] = Map(
    "frameRate" -> 0,
    "graph" -> new Graph(),
    "view" -> "macro",
    "pause" -> false,
    "debug" -> true,
    "selectionRadius" -> 10,
    "scene" -> new Scene()
  )
  start
  
  def act() {


    
    // we could eventually keep a
    //var model = new Model()
    
    while(true) {
      receive {

        case ('updateNode,value) =>
          //context ! 'updateNode -> value

          // main want the scene!
        case "scene" => reply(scene)

        case ('updated,"graph",value:Any,previous:Any) =>
          // log("ignoring update of "+key)
          buildScene
          
        case ('updated,key:String,value:Any,previous:Any) =>
          // log("ignoring update of "+key)
              
          
        case ('openString,str:String) => (new GEXFImporter) ! str
        case ('openURL,url:String) =>
          val buf = new StringBuilder
          io.Source.fromURL(new java.net.URL(url)).foreach(buf.append)
          self ! 'openString -> buf.toString
          
  
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
  def buildScene = {
    var s = new MutableScene()
    val g = get[Graph]("graph")
    //properties("graph")//.asInstance[Graph]
    //val view : String = properties("scene.view") match { case s:String => s }
    
    get[String]("view") match {
      case "macro" =>
        g.nodes.foreach { 
          case n =>
            s.nodes ::= new NodeDrawing(n.position, n.size, n.color, 'Disk)
            s.labels ::= new LabelDrawing(n.position, n.label, n.size.toInt)
            n.links.foreach { case (id,weight) =>
                val m = g.node(id)
                s.edges ::= new EdgeDrawing(n.position,
                   m.position,
                   1,
                   1,
                   (150,150,150), // 
                   16) // depend on the distance relative to screen (use p:PApplet to compute it?)
            }
        }
        println("put "+s.nodes.size+" nodes, "+s.edges.size+" edges into scene")
    
      case "meso" =>
    }
    // send to spatializer
    self ! "scene" -> s.toScene
  }
  
  def get[T](key:String) : T = properties.get(key).get.asInstanceOf[T]

}
