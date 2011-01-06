/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import netscape.javascript.JSObject

class Browser (val window:JSObject=null, val jsContext:String="") {

  val subPrefix = if (jsContext!=null) jsContext else ""
  val apiPrefix = "tinaviz."

  def call(fnc:String,args: Array[Object]=null) : Object = {
    if (true) println("window: "+window+"  call: "+fnc+" args: "+args)
    if (window!=null) window.call(fnc, args) else null
  }

  def setTimeout(message:Array[Object]) : Object = {
    call("setTimeout", message)
  }

  def callAndForget(func:String, args:String="") : Object = {
    val message = Array[Object] (
      subPrefix + apiPrefix + func + "( " + args + ")",
      new java.lang.Integer(0)
    )
    setTimeout(message)
  }

  def buttonStateCallback(attr:String, state:Boolean) = {
    callAndForget("_buttonStateCallback", "'" + attr + "'," +
                  (if (state) "true" else "false"))
  }

  def graphImportedCallback(msg:String) = {
    callAndForget("_graphImportedCallback", msg)
  }

  def init() = {
    callAndForget("_initCallback","")
  }
}
