/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import org.daizoru._

object Renderer extends node.util.Actor {

  start
  
  def act() {
    
    loop {
      react {
        case msg => println("unknow msg: "+msg)
      }
    }
    
  }
}

