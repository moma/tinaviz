/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import org.daizoru._
import eu.tinasoft.tinaviz

import tinaviz.drawing._
import tinaviz.data._
import tinaviz.graph._
import tinaviz.context._

import actors._
import Actor._

class Tinaviz extends node.util.Actor {

  start
  
  def act() {

    var context : Actor = new ContextManager()

    // scene cache
    var scene : Scene = new Scene()

    var properties : Map[Symbol,Any] = Map(
      'frameRate -> 0,
      'pause -> false,
      'debug -> true
    )
    
    // we could eventually keep a
    //var model = new Model()
    
    loop {
      react {

        // called when we have to load a new graph
        case 'load =>
          // kill the previous
          context ! 'exit
          context = new ContextManager()


        // scene builder update the scene!
        case _scene:Scene => scene = _scene

        // main want the scene!
        case 'getScene => reply(scene)


        case ('updated,'frameRate,value:Any,previous:Any) =>
              // don't care about frameRate updates

        case ('updated,'pause,value:Any,previous:Any) =>
              // don't care about pause updates

        case ('updated,key:Symbol,value:Any,previous:Any) =>
              log("ignoring update of "+key)

        case key:Symbol =>
          //if (properties.contains(k))
          reply(properties(key))

        case (key:Symbol,value:Any) =>
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
}
