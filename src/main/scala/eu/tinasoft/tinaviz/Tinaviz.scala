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

    var externals = new Externals()
    // we could eventually keep a
    //var model = new Model()
    
    loop {
      react {

        case ext:Externals =>
          // ext.frameRate

        // called when we have to load a new graph
        case 'load =>
          // kill the previous
          context ! 'exit


          context = new ContextManager()

         

        // scene builder update the scene!
        case _scene:Scene => scene = _scene

        // main want the scene!
        case 'getScene => reply(scene)
        
        case msg => println("unknow msg: "+msg)
      }
    }
    
  }
}
