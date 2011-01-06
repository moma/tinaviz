/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.context

import org.daizoru._
import eu.tinasoft._

import tinaviz.graph._
import tinaviz.drawing._

import actors._
import Actor._

class ContextManager (val graph : Graph = new Graph()) extends node.util.Actor {

  start


  def act() {

    //var model = new Model()

    while(true) {
      receive {

        // generate a new drawing model as background task..
        case 'model =>
          reply(new Model (
              background=(255,255,255)
            ))

        case 'exit => exit
        case msg => log("unknow msg: "+msg)
      }
    }

  }
}
