package eu.tinasoft.tinaviz.io

import actors._
import Actor._
import daizoru._
import eu.tinasoft._
import tinaviz.io.json.Json

object Synchronizer extends node.util.Actor {

  start

  def act() {
    var pending = List.empty[String]
    var busy = false
    while (true) {
      receive {
      
        // query from the applet
        case _pending : List[String] => 
        println("received request from viz")
        if (pending.isEmpty) {
            pending = pending
            
            // when we receive something from the navigator,
            // we call-back our friend
            println("Database: going to query "+pending)
            Browser ! "_callbackGetNeighbourhood" -> (todoList, neighbours)
         }
         

         // result from the browser
         case result : String =>
            println("Database: received result from the browser")

            var newNodeData = Map.empty[String,Map[String,Any]]
            Server ! 'updateNodes -> newNodeData
            pending = List.empty[String] 
           
         case any => println("Database: cannot understand message '"+any+"'")
      }
    }
  }
}
