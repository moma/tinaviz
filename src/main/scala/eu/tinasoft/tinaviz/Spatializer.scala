/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import org.daizoru._

object Spatializer extends node.util.Actor {

  start
  
  def act() {

    while(true) {
      loop {
        react {
          //case
          case msg => println("unknow msg: "+msg)
        }
      }
    }

    
  }
}
