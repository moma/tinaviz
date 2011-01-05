/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import org.daizoru._
import eu.tinasoft._

import tinaviz.drawing._

class MainController extends node.util.Actor {

  start
  
  def act() {
    
    var model = new Model()
    
    loop {
      react {
        case 'update => 
           model = new Model()
           // TODO modify model here
        
        case 'model =>
          reply(model)
          
        case msg => println("unknow msg: "+msg)
      }
    }
    
  }
}
