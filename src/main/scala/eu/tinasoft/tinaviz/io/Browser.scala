/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.io

import org.daizoru._
import eu.tinasoft.tinaviz._

import eu.tinasoft.tinaviz.io.json.Json
import java.applet.Applet
import netscape.javascript.JSObject
import eu.tinasoft.tinaviz.io.json.Base64

object Browser extends node.util.Actor {

  private var _window:JSObject = null
  private var _subPrefix = ""
  private var _apiPrefix = "tinaviz."

  def init(applet:Applet=null, jsContext:String=null) = {
    start
    println("jsContext: "+jsContext)
    this._window = try { JSObject.getWindow(applet) } catch { case x => null }
    this._subPrefix = if (jsContext!=null) jsContext else ""
    this._apiPrefix = "tinaviz."

    this ! "_initCallback"
  }
  private def buttonStateCallback(attr:String, state:Boolean) = {
    //callAndForget("_buttonStateCallback", "'" + attr + "'," +
    //             (if (state) "true" else "false"))
  }

  private def graphImportedCallback(msg:String) = {
    // callAndForget("_graphImportedCallback", msg)
  }

  def replace(str:String) = {
     str.replaceAll("\"","\\\"").replace("'","\\'")
  }
  def act() {

    //var model = new Model()

    loop {
      react {

        // asynchronous call
        case func:String =>
          println("ASYNC window.call: "+_subPrefix + _apiPrefix + func+"")
          if (_window!=null) {
            _window.call("setTimeout", Array[Object] (_subPrefix + _apiPrefix + func+"()",new java.lang.Integer(0)))
          }

         case (func:String,(any1,any2)) =>
          if (_window!=null) {
            println("calling setTimeout(\""+_subPrefix + _apiPrefix + func+"('"+replace(Json.build(any1).toString)+"','"+replace(Json.build(any2).toString)+"')\")")
            _window.call("setTimeout", Array[Object] (_subPrefix + _apiPrefix +func+"('"+replace(Json.build(any1).toString)+"','"+replace(Json.build(any2).toString)+"')",new java.lang.Integer(0)))
          }

        case (func:String,(any1,any2,any3)) =>
         val args = Array[Object] (
           Json.build(any1).toString,
           Json.build(any2).toString,
           Json.build(any3).toString,
           new java.lang.Integer(0))
         println("SYNC window.call: "+_subPrefix + _apiPrefix + func+"("+args+")")
         if (_window!=null) {
           _window.call(_subPrefix + _apiPrefix + func, args)
         }

        case (func:String,(any1,any2,any3,any4)) =>
         val args = Array[Object] (
           Json.build(any1).toString,
           Json.build(any2).toString,
           Json.build(any3).toString,
           Json.build(any4).toString,
           new java.lang.Integer(0))
         println("SYNC window.call: "+_subPrefix + _apiPrefix + func+"("+args+")")
         if (_window!=null) {
           _window.call(_subPrefix + _apiPrefix + func, args)
         }

        case ('forceDownload, str:String) => 
            val base64ified = "data:application/xml;base64,"+Base64.encode(str)
            _window.call("open", Array[Object] (base64ified,"_newtab",new java.lang.Integer(0)))
        
        case (func:String,any) =>
          val args = Array[Object] (
            Json.build(any).toString,
            new java.lang.Integer(0))
          println("SYNC window.call: "+_subPrefix + _apiPrefix + func+"("+args+")")
          if (_window!=null) {
           // _window.call(_subPrefix + _apiPrefix + func, args)
            _window.call("setTimeout", Array[Object] (_subPrefix + _apiPrefix +func+"('"+replace(Json.build(any).toString)+"')",new java.lang.Integer(0)))
          }
          /*
           case (func:String,any) =>
           val json = Json.build(any).toString
           val args = Array[Object] (Json.build(any).toString,new java.lang.Integer(0))
           println("window.call: "+_subPrefix + _apiPrefix + func+"("+args+")")
           if (_window!=null) {
           _window.call(_subPrefix + _apiPrefix + func, args)
           }
           */
        case msg => log("unknow msg: "+msg)
      }
    }
  }



}
