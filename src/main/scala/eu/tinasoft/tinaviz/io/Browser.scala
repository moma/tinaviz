/************************************************************************
                                  Tinaviz
*************************************************************************
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

package eu.tinasoft.tinaviz.io

import eu.tinasoft.tinaviz._

import eu.tinasoft.tinaviz.io.json.Json
import java.applet.Applet
import netscape.javascript.JSObject
import eu.tinasoft.tinaviz.io.json.Base64
import actors.Actor
import actors.Actor._

object Browser extends Actor {

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

  def replace(str:String) = {
     str.replaceAll("\"","\\\"").replace("'","\\'")
  }

  def act() {
    loop {
      react {
        case 'exit =>
          println("exiting browser")
          exit()

        // asynchronous call
        case func:String =>
          //println("ASYNC window.call: "+_subPrefix + _apiPrefix + func+"")
          if (_window!=null) {
            _window.call("setTimeout", Array[Object] (_subPrefix + _apiPrefix + func+"()",new java.lang.Integer(0)))
          }

         case (func:String,(any1,any2)) =>
          if (_window!=null) {
            //println("calling setTimeout(\""+_subPrefix + _apiPrefix + func+"('"+replace(Json.build(any1).toString)+"','"+replace(Json.build(any2).toString)+"')\")")
            _window.call("setTimeout", Array[Object] (_subPrefix + _apiPrefix +func+"('"+replace(Json.build(any1).toString)+"','"+replace(Json.build(any2).toString)+"')",new java.lang.Integer(0)))
          }

        case (func:String,(any1,any2,any3)) =>
         val args = Array[Object] (
           Json.build(any1).toString,
           Json.build(any2).toString,
           Json.build(any3).toString,
           new java.lang.Integer(0))
         //println("SYNC window.call: "+_subPrefix + _apiPrefix + func+"("+args+")")
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
         //println("SYNC window.call: "+_subPrefix + _apiPrefix + func+"("+args+")")
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
          //println("SYNC window.call: "+_subPrefix + _apiPrefix + func+"("+Json.build(any).toString+")")
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
        case msg => println("Browser: unknow msg: "+msg)
      }
    }
  }



}
