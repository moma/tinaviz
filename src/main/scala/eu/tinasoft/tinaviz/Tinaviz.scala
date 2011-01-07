/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import actors._
import eu.tinasoft.tinaviz.data.json.JsonParser

trait Tinaviz {
  
  val tinaviz : Actor = new TinavizActor()
  
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
   * 
   */
  def getNeighbourhood(view:String, rawJSONList:String) : Unit = {
    
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
}
