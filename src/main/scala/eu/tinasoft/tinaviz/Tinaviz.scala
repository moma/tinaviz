/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import actors._
import eu.tinasoft.tinaviz.data.json.JsonParser
import scala.util.parsing.json.JSONObject

trait Tinaviz {
  
  val tinaviz : Actor = new TinavizActor()


  private var cached : Map[String,(Any,Future[Any])] = Map.empty

  /**
   * Regularly get a param using future
   *
   */
  protected def getIfPossible[T](key:String) : T = {
    val tp = cached(key)
    var value : T = tp._1 match {
      case t:T => return t
      case err => throw new Exception("error, key "+key+" has no default")
    }

    var future : Future[T] = null
    // if a future is already on the rails, don't send a message, just wait
    tp._2 match {
      case null => (tinaviz !! key) match { case f:Future[T] => future = f }
      case f:Future[T] => future = f
    }

    if (future.isSet) {
      value = future()
      cached += key -> (value,null)
    }
    value
  }

  protected def setDefault(key:String,value:Any) = {
    cached += key -> (value,null)
  }
  // Called by Javascript

  def setPause(b:Boolean) = {
    true
  }
  
  def setView(s:String) = {
    s match {
      case "macro" =>
      case "meso" =>
    }
    true
  }

  def togglePause = {
    (tinaviz !? "pause" -> 'toggle) match {
      case b:Boolean => b
      case x => false
    }
  }
  /**
   * Deprecated
   */
  def toggleNodes = {
    true
  }
  /**
   * Deprecated
   */
  def toggleEdges = {
    true
  }
  /**
   * Deprecated
   */
  def toggleLabels = {
    true
  }
  /**
   * Deprecated
   */
  def toggleHD = {
    
  }

  def unselect = {

  }

  /**
   * Set a param
   * TODO: boolean sync?
   */
  def setParam(key:String,value:String,sync:Boolean) = {
    tinaviz ! key -> value
    println("ignoring sync: "+sync)
  }

  
  /**
   * Update a Node in the current view, from it's UUID
   */
  def updateNode(str:String) = {
    // we help the JS developer by telling him if the JSON is valid or not
    val res = JsonParser.parse(str)
    
    if (!res.isDefined) throw new IllegalArgumentException("Error, invalid JSON!")
    tinaviz ! 'updateNode -> res.get
    
  }
  
  /**
   * TODO since this is blocking, we should move it elsewhere
   */
  def openURI(url:String) = {
    tinaviz ! 'openURL -> url
  }
  
  def openString(str:String) = {
    tinaviz ! 'openString -> str
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
