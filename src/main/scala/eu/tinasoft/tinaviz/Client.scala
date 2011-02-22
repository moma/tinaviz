/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.Server

import actors._
import Actor._
import eu.tinasoft._
import tinaviz.Server

trait Client {
  
  private var cached : Map[String,(Any,Future[Any])] = Map.empty

  /**
   * Regularly get a param using future
   *
   */
  protected def getIfPossible[T](key:String) : T = {
    val tp = cached(key)
    var value : T = tp._1 match {
      case t:T =>  if (t!=null) t else throw new Exception("error, "+key+" default is null")
      case err => throw new Exception("error, key "+key+" has no default")
    }

    var future : Future[T] = null
    // if a future is already on the rails, don't send a message, just wait
    tp._2 match {
      case null =>
        //if (key.equals("scene")) println("asking for future!")
        (Server !! key) match {
          case f:Future[T] =>
            //if (key.equals("scene")) println("got future! storing it..")
            future = f
            cached += key -> (value,future)
        }
      case f:Future[T] =>
        future = f
    }

    if (future.isSet) {
      //if (key.equals("scene")) println("future has new value!")
      val x = future()
      // we never want nulls
      if (x!=null) {
        value = x
      } 
      // else no luck.. maybe more next time!
      
      //if (key.equals("scene")) println("reseting future value")
      cached += key -> (value,null)
    }
    value
  }

  protected def setDefault(key:String,value:Any) = {
    cached += key -> (value,null)
  }
// Called by Javascript

  def togglePause = {
    (Server !? "pause" -> 'toggle) match {
      case b:Boolean => b
      case x => false
    }
  }

  def select(id:String) = {
    println("JavaScript asked for select("+id+")")
    Server ! 'select -> id
    true
  }
  def unselect = {
    Server ! 'unselect
    true
  }

  // var Map[String]
  /**
   * Set a param
   * TODO: boolean sync?
   */
  /*
   def getLater(cb:String,key:String) = {
   //val subscriber = sender
   println("getLater(cb: "+cb+", key: "+key)
   val sessionUser = actor {
   //loop {
   receive {
   case any =>
   Browser ! "callCb" -> Map( "cb" -> cb, "data" -> (Server !? key))
   }
   //}
   }
   sessionUser ! 'go

   }
   */
  // { action: unselect }

  def openURI(url:String) = {
      Server ! 'open -> new java.net.URL(url)
    true
  }
  def openString(str:String) = {
      Server ! 'open -> str
    true
  }
  def set(key:String, value:Any) = {
    println("JavaScript asked for set("+key+","+value+")")
    Server ! key -> value
    true
  }

  def setAs(key:String, value:java.lang.Object, t:String) = {
    println("JavaScript asked for setAs(key:"+key+", value:"+value+", t:"+t+")")
    t match {
       case "Int" => 
       Server ! key -> value.toString.toInt
       case "Float" => 
       Server ! key -> value.toString.toFloat
       case "Double" => 
       Server ! key -> value.toString.toDouble
       case "Boolean" =>
        println("converting "+key+" : "+value+" to Boolean")
        Server ! key -> value.toString.toBoolean
       case "String" => Server ! key -> value.toString
       case x => Server ! key -> value
    }
    null
  }
  def get(key:String) = {
    println("JavaScript asked of get("+key+")")
    (Server !? key)
  }
  /*
   def msgCb(action:String, args:String, cbId:Int) {
   println("msgArgCb - action: "+action+" cbId:"+cbId+" args: "+args)

   val cb = actor {
   receive {
   case msg => Browser ! "callCb" -> Map( "cb" -> cbId, "data" -> msg)
   }
   }
   Server ! ('js, action, Json.parse(args), cb)
   }*/

  // warning: synchronous..


  /**
   * Update a Node in the current view, from it's UUID
   */
  def updateNode(id:String, str:String) = {
    // we help the JS developer by telling him if the JSON is valid or not
    val res = JsonParser.parse(str)
    
    if (!res.isDefined) throw new IllegalArgumentException("Error, invalid JSON!")
    Server ! 'updateNode -> res.get
    
  }
  
  def selectByPattern(pattern:String, patternMode:String) : Unit = {
    
    if (pattern == null || patternMode == null) {
      System.out.println("selectByPattern(" + pattern + ", " + patternMode + ")");
      return;
    }
    // shutdown the "center on visualization" mode

    if (pattern.isEmpty()) {
      return;
    }
    Server ! 'selectByPattern -> pattern
  }
  
  def highlightByPattern(pattern:String, patternMode:String) : Unit = {
    
    if (pattern == null || patternMode == null) {
      System.out.println("highlightByPattern(" + pattern + ", " + patternMode + ")");
      return;
    }
    // shutdown the "center on visualization" mode

    if (pattern.isEmpty()) {
      return;
    }
    Server ! 'highlightByPattern -> pattern
  }
  
  /**
   * TODO fix it (blocking call!)
   */
  def getNodeAttributes(view:String, uuid:String) : String = {
    //System.out.println("getting node by UUID: " + uuid)
    val attributes : String = (Server !? 'getNodeAttributes -> uuid) match {
      case m:Map[Any,Any] =>
         Json.build(m).toString

      case any =>
        throw new Exception("couldn't find node attributes "+uuid)
    }
    println("TODO send the node attributes "+attributes+" to the client")
    attributes
  }

  def getNeighbourhood(view:String, rawJSONList:String) : String = {

    val todoList = Json.parse(rawJSONList)

    println("TODO get the neighbourListof "+todoList+"")
    val neighbours = (Server !? ('getNeighbourhood,view,todoList)) match {
      case m:Map[Any,Map[String,Any]] =>
          println("TODO building neighbours JSON reply..")
         Json.build(m).toString

      case any =>
        throw new Exception("couldn't find neighbourList for any of "+rawJSONList)
    }
    // neighbourList
    Browser ! "_callbackGetNeighbourhood" -> (todoList, neighbours)

    // _callbackGetNeighbourhood = function(selection_list_str,neighbour_node_list_str) {
    ""
  }
  
  // TODO should be asynchronous
  def getNodes(view:String, category:String) : String = {
    //System.out.println("getting node by UUID: " + uuid)
    val nodes : String = (Server !? ('getNodes,view,category)) match {
      case m:Map[String,Map[String,Any]] =>
        println("Server replied with some node map: "+m)
        //Json.build(m).toString
        Json.build(m.map{case(k,v)=>v}.toList).toString
      case any =>
        throw new Exception("couldn't find nodes from view: "+view+" and category: "+category)
    }
    println("TODO send the nodes "+nodes+" to the client")
    //Browser ! "_callbackGetNodes" -> nodes
    nodes
  }
}
