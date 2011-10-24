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
import xml.dtd.PublicID
import reflect.{BooleanBeanProperty, ValDef}

class Webpage(val session: Session) extends Actor {

  lazy val (connected, window, jsContext, prefix) : (Boolean, JSObject, String, String) = try {
    (true, JSObject.getWindow(session.applet), session.applet.getParameter("js_context"), "tinaviz.")
  } catch {
    case e => (false, null, "", "")
  }

  private def buttonStateCallback(attr: String, state: Boolean) = {
    //callAndForget("_buttonStateCallback", "'" + attr + "'," +
    //             (if (state) "true" else "false"))
  }

  def replace(str: String) = {
    str.replaceAll("\"", "\\\"").replace("'", "\\'")
  }

  def act() {
    val delay = 200 // set timeout delay
    this ! "_initCallback"
    loop {
      react {
        case 'exit =>
          println("exiting browser")
          exit()

        // asynchronous call
        case func: String =>
          //println("ASYNC window.call: " + jsContext + prefix + func + "")
          if (window != null) {
            window.call("setTimeout", Array[Object](jsContext + prefix + func + "()", new java.lang.Integer(delay), new java.lang.Integer(0)))
          }

        case (func: String, (any1, any2)) =>
          if (window != null) {
            //println("calling setTimeout(\"" + jsContext + prefix + func + "('" + replace(Json.build(any1).toString) + "','" + replace(Json.build(any2).toString) + "')\")")
            window.call("setTimeout", Array[Object](jsContext + prefix + func + "('" + replace(Json.build(any1).toString) + "','" + replace(Json.build(any2).toString) + "')", new java.lang.Integer(0)))
          }

        case (func: String, (any1, any2, any3)) =>
          val args = Array[Object](
            Json.build(any1).toString,
            Json.build(any2).toString,
            Json.build(any3).toString,
            new java.lang.Integer(0))
          //println("SYNC window.call: " + jsContext + prefix + func + "(" + args + ")")
          if (window != null) {
            window.call(jsContext + prefix + func, args)
          }

        case (func: String, (any1, any2, any3, any4)) =>
          val args = Array[Object](
            Json.build(any1).toString,
            Json.build(any2).toString,
            Json.build(any3).toString,
            Json.build(any4).toString,
            new java.lang.Integer(0))
          //println("SYNC window.call: "+_subPrefix + _apiPrefix + func+"("+args+")")
          if (window != null) {
            window.call(jsContext + prefix + func, args)
          }

        case ('forceDownload, str: String) =>
          val base64ified = "data:application/gexf;base64," + Base64.encode(str)
          window.call("open", Array[Object](base64ified, "_newtab", new java.lang.Integer(0)))

        case ('cb, cbId:String, any) =>
          val args = Array[Object](jsContext + prefix + "callCallback" + "('"+cbId+"', '" + replace(Json.build(any).toString) + "')", new java.lang.Integer(delay), new java.lang.Integer(0))
          println("SYNC window.call: " + args + "")
          if (window != null) {
            window.call("setTimeout", args)
          }

        case (func: String, any) =>
          val args = Array[Object](jsContext + prefix + func + "('" + replace(Json.build(any).toString) + "')", new java.lang.Integer(delay), new java.lang.Integer(0))
          println("SYNC window.call: " + args + "")
          if (window != null) {
            window.call("setTimeout", args)
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
        case msg => println("Webpage: unknow msg: " + msg)
      }
    }
  }


}
