/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import netscape.javascript.JSObject

object Browser {

  private var _window:JSObject = null
  private var _subPrefix = ""
  private var _apiPrefix = "tinaviz."

  def config(window:JSObject=null, jsContext:String=null) = {
    println("jsContext: "+jsContext)
    this._window = window
    val _subPrefix = if (jsContext!=null) jsContext else ""
    val _apiPrefix = "tinaviz."
  }


  def call(fnc:String,args: Array[Object]=null) : Object = {
    if (true) println("window: "+_window+"  call: "+fnc+" args: "+args)
    if (_window!=null) _window.call(fnc, args) else null
  }

  def setTimeout(message:Array[Object]) : Object = {
    call("setTimeout", message)
  }

  def callAndForget(func:String, args:String="") : Object = {
    val message = Array[Object] (
      _subPrefix + _apiPrefix + func + "( " + args + ")",
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
