/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.data

import scala.util.parsing.json.JSON
import scala.util.parsing.json.JSONObject

/**
 * TODO implicit conversion between json to map
 */
class JSONImporter(val rawJSON:String) {

  def toMap = {
    var map = Map[String,Any]()
    val x = JSON.parse(rawJSON)
  }

  /**
   * TODO not finished
   */
  def mapToBase64(map:Map[Any,Any]) = {
    val json = new JSONObject(map)
    json.toString

  }
}
