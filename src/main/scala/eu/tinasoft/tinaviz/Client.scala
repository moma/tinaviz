/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import actors._
import Actor._
import eu.tinasoft._
import tinaviz.io.JsonParser
import eu.tinasoft.tinaviz.io.Browser
import eu.tinasoft.tinaviz.io.json.Json
import scala.util.parsing.json.JSONObject

trait Client {
  
  val tinaviz : Actor = new Server()


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
        (tinaviz !! key) match {
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
    (tinaviz !? "pause" -> 'toggle) match {
      case b:Boolean => b
      case x => false
    }
  }

  def select(id:String) = {
    tinaviz ! 'select -> id
    true
  }
  def unselect = {
    tinaviz ! 'unselect
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
   Browser ! "callCb" -> Map( "cb" -> cb, "data" -> (tinaviz !? key))
   }
   //}
   }
   sessionUser ! 'go

   }
   */
  // { action: unselect }

  def openURI(url:String) = {
      tinaviz ! 'open -> new java.net.URL(url)
  }
  def openString(str:String) = {
      tinaviz ! 'open -> str
  }
  def set(key:String, value:Any) {
    tinaviz ! key -> value
  }
  def setAs(key:String, value:Any, t:String) {
    t match {
       case "Int" => tinaviz ! key -> value.toString.toInt
       case "Float" => tinaviz ! key -> value.toString.toFloat
       case "Double" => tinaviz ! key -> value.toString.toDouble
       case "Boolean" => tinaviz ! key -> value.toString.toBoolean
       case "String" => tinaviz ! key -> value.toString
       case x => tinaviz ! key -> value
    }
  }
  def get(key:String) = {
    (tinaviz !? key)
  }
  /*
   def msgCb(action:String, args:String, cbId:Int) {
   println("msgArgCb - action: "+action+" cbId:"+cbId+" args: "+args)

   val cb = actor {
   receive {
   case msg => Browser ! "callCb" -> Map( "cb" -> cbId, "data" -> msg)
   }
   }
   tinaviz ! ('js, action, Json.parse(args), cb)
   }*/

  // warning: synchronous..


  /**
   * Update a Node in the current view, from it's UUID
   */
  def updateNode(id:String, str:String) = {
    // we help the JS developer by telling him if the JSON is valid or not
    val res = JsonParser.parse(str)
    
    if (!res.isDefined) throw new IllegalArgumentException("Error, invalid JSON!")
    tinaviz ! 'updateNode -> res.get
    
  }
  
  def selectNodesByLabel(matchLabel:String, matchCategory:String, matchMode:String, viewToSearch:String, center:Boolean) : Unit = {
    if (viewToSearch == null || matchLabel == null || matchCategory == null || matchMode == null) {
      System.out.println("selectNodesByLabel(" + matchLabel + ", " + matchCategory + ", " + matchMode + ", " + viewToSearch + ", " + center + ")");
      return;
    }
    // shutdown the "center on visualization" mode

    if (matchLabel.isEmpty()) {
      return;
    }

    if (viewToSearch.equalsIgnoreCase("visualization") || viewToSearch.isEmpty()) {
      System.out.println("calling selectFromNodes on output graph..");
      //selectFromNodes(
      //        getView().getOutputGraph().getNodesByLabel(
      //        matchLabel, matchMode));
    } else {
      System.out.println("calling selectFromNodes on any graph..");
      //selectFromNodes(
      //        getView(viewToSearch).getNodesByLabel(
      //        matchLabel, matchCategory, matchMode));
    }

    //Visualization.centerOnSelection = center;
    //redrawLater();


  }
  

  /**
   * TODO fix it (blocking call!)
   */
  def getNodeAttributes(view:String, uuid:String) : String = {
    //System.out.println("getting node by UUID: " + uuid)
    val node = (tinaviz !? 'getNodeAttributes -> uuid) match {
      case m:Map[Any,Any] => return new JSONObject(m).toString
    }
    throw new Exception("couldn't find node attributes "+uuid)
  }

  def getNeighbourhood(view:String, rawJSONList:String) : Unit = {
    var neighbourList = "{}"
    if (view == null) {
      //Console.log("getNeighbourhood: view is null");
      return;
    }
    if (rawJSONList == null) {
      //Console.log("getNeighbourhood: id is null");
      return;
    }
    //Console.log("getNeighbourhood(" + view + ", " + rawJSONList + ")");
    try {
      // neighbourList = (view.isEmpty() | view.equalsIgnoreCase("current"))
      //   ? getView().getOutputGraph().getNeighbourhoodAsJSON(rawJSONList)
      //   : getView(view).getInputGraph().getNeighbourhoodAsJSON(rawJSONList);
    } catch  {
      case ex:Exception =>
        throw new Exception(ex)
        // Console.error("getNeighbourhood error: " + ex)
    }
    if (neighbourList == null | neighbourList.isEmpty() | neighbourList.equals("{}")) {
      //Console.log("getNeighbourhood: ERROR, json export failed: " + neighbourList);
      return;
    }

    // Browser ! "_callbackGetNeighbourhood" -> "'" + getSelectedNodesJSON(view) + "','" + neighbourList + "'"
  }
}
