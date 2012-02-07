/************************************************************************
                                  Tinaviz
 * ************************************************************************
 This application is part of the Tinasoft project: http://tinasoft.eu
 Tinaviz main developer: julian.bilcke @ iscpif.fr  (twitter.com/flngr)

 Copyright (C) 2009-2011 CREA Lab, CNRS/Ecole Polytechnique UMR 7656 (Fr)

 This program is free software: you can redistribute it and/or modify it
 under the terms of the GNU General Public License as published by the
 Free Software Foundation, either version 3 of the License, or (at your
 option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program. If not, see <http://www.gnu.org/licenses/>.
 ************************************************************************/

package eu.tinasoft.tinaviz

import actors._
import Actor._
import actors.Futures._

import eu.tinasoft._
import tinaviz.io.json.Json
import tinaviz.io.Webpage

trait Client {

  private var cached: Map[String, (Any, Future[Any])] = Map.empty

  private var session: Session = null

  def setClientSession(s: Session) = {
    session = s
  }

  /**
   * Regularly get a param using future
   *
   */
  protected def getIfPossible[T](key: String): T = {
    val tp = cached(key)
    var value: T = tp._1 match {
      case t: T => if (t != null) t else throw new Exception("error, " + key + " default is null")
      case err => throw new Exception("error, key " + key + " has no default")
    }

    var future: Future[T] = null
    // if a future is already on the rails, don't send a message, just wait
    tp._2 match {
      case null =>
        //if (key.equals("scene")) println("asking for future!")
        (session.server !! key) match {
          case f: Future[T] =>
            //if (key.equals("scene")) println("got future! storing it..")
            future = f
            cached += key ->(value, future)
        }
      case f: Future[T] =>
        future = f
    }

    if (future.isSet) {
      //if (key.equals("scene")) println("future has new value!")
      val x = future()
      // we never want nulls
      if (x != null) {
        value = x
      }
      // else no luck.. maybe more next time!

      //if (key.equals("scene")) println("reseting future value")
      cached += key -> (value, null)
    }
    value
  }

  protected def setDefault(key: String, value: Any) = cached += key -> (value, null)

  def openURI(cb:String, url: String)    = session.server ! cb.toInt -> ('open, new java.net.URL(url))
  def openString(cb:String, str: String) = session.server ! cb.toInt -> ('open, str)

  def sendSetTuple2(cb: String, key: String, value1: java.lang.Object, value2: java.lang.Object, t: String) {

    // println("-> sendTuple2(key:" + key + ", value1:" + value1 + ", value2: " + value2 + ", t:" + t + ")")

    //"[\"NGram::41a14ef0a30a812946b69d522e1570db9e4c0d5579753ba429e7291a9bdbc96c\",\"NGram::bbbf7a6412d6d3e8244ac1fda5e35a20037acee661288cb95b7b18cf469980aa\",\"NGram::bc020a35b7f9cb1382e7b534c68e3c531d849b119bf14f75ddead6cc45c3ccc1\"]"
    session.server ! cb.toInt -> (key, t match {
      case "Int" => (value1.toString.toInt,value2.toString.toInt)
      case "Float" => (value1.toString.toFloat,value2.toString.toFloat)
      case "Double" => (value1.toString.toDouble,value2.toString.toDouble)
      case "Boolean" => (value1.toString.toBoolean,value2.toString.toBoolean)
      case "String" => (value1.toString,value1.toString)
      case "Json" => (Json.parse(value1.toString),Json.parse(value2.toString))
      case "Any" => (value1,value2)
      case x => (value1,value2)
    })
  }

  def sendSet(cb: String, key: String, value: java.lang.Object, t: String) {

    //println("-> send(cb: "+cb+", key:" + key + ", value:" + value + ", t:" + t + ")")
    //"[\"NGram::41a14ef0a30a812946b69d522e1570db9e4c0d5579753ba429e7291a9bdbc96c\",\"NGram::bbbf7a6412d6d3e8244ac1fda5e35a20037acee661288cb95b7b18cf469980aa\",\"NGram::bc020a35b7f9cb1382e7b534c68e3c531d849b119bf14f75ddead6cc45c3ccc1\"]"
    session.server ! cb.toInt -> (key, t match {
      case "Int" => value.toString.toInt
      case "Float" => value.toString.toFloat
      case "Double" => value.toString.toDouble
      case "Boolean" => value.toString.toBoolean
      case "String" => value.toString
      case "Json" => Json.parse(value.toString)
      case x => value
    })
  }


  def sendGet(cb: String, key: String, t: String) {
    //println("-> sendGet(cbId: "+cb+", key: "+key+", t: "+t+")")
    session.server ! cb.toInt -> key
  }

  /**
   * Update a Node in the current view, from it's UUID
   */
  def updateNode(id: String, str: String) = {
    // we help the JS developer by telling him if the JSON is valid or not
    // val res = JsonParser.parse(str)

    //if (!res.isDefined) throw new IllegalArgumentException("Error, invalid JSON!")
    //Server ! 'updateNode -> res.get

  }

  def selectByPattern(cb:String, pattern: String, patternMode: String): Unit = {

    if (pattern == null || patternMode == null) {
      println("selectByPattern(" + pattern + ", " + patternMode + ")");
      return;
    }
    // shutdown the "center on visualization" mode

    if (pattern.isEmpty()) {
      return;
    }
    session.workflow ! cb.toInt -> ("selectByPattern", pattern)
  }


  def selectByNeighbourPattern(cb:String,pattern: String, patternMode: String, category: String): Unit = {

    if (pattern == null || patternMode == null) {
      println("selectByNeighbourPattern(" + pattern + ", " + patternMode + ", " + category + ") null");
      return;
    }
    // shutdown the "center on visualization" mode

    if (pattern.isEmpty()) {
      return;
    }
    session.workflow ! cb.toInt -> ("selectByNeighbourPattern", pattern, category)
  }

  /**
   * Select a node from its ID
   *
   * obsolete - replaced by JS code
   *
   * def select(uuid:String) : Unit = Server ! 'select -> uuid
   */

  def highlightByPattern(cb:String, pattern: String, patternMode: String) {

    if (pattern == null | patternMode == null) {
      println("highlightByPattern(" + pattern + ", " + patternMode + ")");
      return;
    }
    // shutdown the "center on visualization" mode

    if (pattern.isEmpty()) {
      return;
    }
    session.workflow ! cb.toInt -> ("highlightByPattern", pattern)
  }

  /**
   * TODO fix it (blocking call!)
   */
  def getNodeAttributes(cb : String, view: String, uuid: String) {
    //System.out.println("getting node by UUID: " + uuid)
    session.workflow ! cb.toInt ->('getNodeAttributes, uuid)
  }

  /**
   * Problem: this is called with a list of 'selection' id (always 0, 1, 2, 3..)
   */
  def getNeighbourhood(cb:String, view: String, rawJSONList: String) {
    val todoList = "selection" //Json.parse(rawJSONList)
    //println("TODO get the neighbourListof "+todoList+" ("+rawJSONList+")")
    session.workflow ! cb.toInt -> ('getNeighbourhood, view, todoList)
  }

  // TODO should be asynchronous
  def getNodes(cb : String, view: String, category: String) {
    session.workflow ! cb.toInt -> ('getNodes, view, category)
  }
}
