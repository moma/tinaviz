/************************************************************************
                                  Tinaviz
*************************************************************************
 This application is part of the Tinasoft project: http://tinasoft.eu
 Tinaviz main developer: julian.bilcke @ iscpif.fr  (twitter.com/flngr)

Copyright (C) 2009-2011 CREA Lab, CNRS/Ecole Polytechnique UMR 7656 (Fr)
************************************************************************/

package eu.tinasoft.tinaviz

import actors._
import Actor._
import eu.tinasoft._
import tinaviz.io.json.Json
import tinaviz.io.Browser

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

  def openURI(url:String) = {
      Server ! 'open -> new java.net.URL(url)
    true
  }
  def openString(str:String) = {
      Server ! 'open -> str
    true
  }

  /**
   * Set a tuple of size two (both elements must be of the same type)
   */
    def sendTuple2(key:String, value1:java.lang.Object, value2:java.lang.Object, t:String) : Unit = {
    //println("-> setAs(key:"+key+", value:"+value+", t:"+t+")")
    t match {
       case "Int" =>
       Server ! key -> (value1.toString.toInt, value2.toString.toInt)
       case "Float" =>
       Server ! key -> (value1.toString.toFloat, value2.toString.toFloat)
       case "Double" =>
       Server ! key -> (value1.toString.toDouble, value2.toString.toDouble)
       case "Boolean" =>
        println("converting "+key+" : ("+value1+","+value2+") to Boolean")
        Server ! key -> (value1.toString.toBoolean,value2.toString.toBoolean)
       case "String" => Server ! key -> (value1.toString, value2.toString)
       case "Json" =>
         val data = (Json.parse(value1.toString),Json.parse(value2.toString))
         //println("parsed Json to "+data)
         Server ! key -> data
       case x => Server ! key -> (value1, value2)
    }
  }

  def send(key:String, value:java.lang.Object, t:String) : Unit = {
    //println("-> setAs(key:"+key+", value:"+value+", t:"+t+")")
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
       case "Json" =>
         val data = Json.parse(value.toString)
         //println("parsed Json to "+data)
         Server ! key -> data
       case x => Server ! key -> value
    }
  }
  def get(key:String) : java.lang.Object = {
    //println("-> get("+key+")")
    (Server !? key).asInstanceOf[AnyRef]
  }

  /**
   * Update a Node in the current view, from it's UUID
   */
  def updateNode(id:String, str:String) = {
    // we help the JS developer by telling him if the JSON is valid or not
   // val res = JsonParser.parse(str)
    
    //if (!res.isDefined) throw new IllegalArgumentException("Error, invalid JSON!")
    //Server ! 'updateNode -> res.get
    
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
    Server ! "selectByPattern" -> pattern
  }


  def selectByNeighbourPattern(pattern:String, patternMode:String, category:String) : Unit = {

    if (pattern == null || patternMode == null) {
      System.out.println("selectByNeighbourPattern(" + pattern + ", " + patternMode + ", "+category+")");
      return;
    }
    // shutdown the "center on visualization" mode

    if (pattern.isEmpty()) {
      return;
    }
    Server ! ("selectByNeighbourPattern",pattern,category)
  }

  /**
   * Select a node from its ID
   *
   * obsolete - replaced by JS code
   *
   * def select(uuid:String) : Unit = Server ! 'select -> uuid
   */

  def highlightByPattern(pattern:String, patternMode:String) : Unit = {
    
    if (pattern == null | patternMode == null) {
      System.out.println("highlightByPattern(" + pattern + ", " + patternMode + ")");
      return;
    }
    // shutdown the "center on visualization" mode

    if (pattern.isEmpty()) {
      return;
    }
    Server ! "highlightByPattern" -> pattern
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

  /**
   * Problem: this is called with a list of 'selection' id (always 0, 1, 2, 3..)
   */
  def getNeighbourhood(view:String, rawJSONList:String) = {
    val todoList = "selection" //Json.parse(rawJSONList)
    println("TODO get the neighbourListof "+todoList+" ("+rawJSONList+")")
    Server ! ('getNeighbourhood,view,todoList)
    true
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
