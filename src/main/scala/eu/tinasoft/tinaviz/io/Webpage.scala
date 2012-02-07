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

  val CB_INIT = 0
  val CB_IMPORTED = 1
  val CB_CLICK = 2
  val CB_VIEW = 3

  lazy val (connected, window, jsContext, prefix) : (Boolean, JSObject, String, String) = try {
    (true, JSObject.getWindow(session.applet), session.applet.getParameter("js_context"), "tinaviz.")
  } catch {
    case e => (false, null, "", "")
  }

  def replace(str: String) = {
    str.replaceAll("\"", "\\\"").replace("'", "\\'")
  }

  def act() {
    val cblatency = 1 // decrease this to make the application faster
    this ! CB_INIT -> Map()
    while (true) {
      receive {
        case 'exit => println("exiting browser"); exit()

        case ('download, str: String) =>
          val base64ified = "data:application/gexf;base64," + Base64.encode(str)
          window.call("open", Array[Object](base64ified, "_newtab", new java.lang.Integer(0)))

        case (cb:Int, data:Map[String,Any]) =>
          val args = Array[Object]("callCallback" + "('"+cb+"', '" + replace(Json.build(data).toString) + "')", new java.lang.Integer(cblatency), new java.lang.Integer(0))
          //println("ASYNC/CB window.call: " + Json.build(data).toString + "")
          if (window != null) window.call("setTimeout", args)

        case msg => println("Webpage: unknow msg: " + msg)
      }
    }
  }


}
