/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import org.daizoru._

import netscape.javascript.JSObject

object Browser extends node.util.Actor {

  start

  private var _window:JSObject = null
  private var _subPrefix = ""
  private var _apiPrefix = "tinaviz."

  def init(window:JSObject=null, jsContext:String=null) = {
    println("jsContext: "+jsContext)
    this._window = window
    val _subPrefix = if (jsContext!=null) jsContext else ""
    val _apiPrefix = "tinaviz."
    self ! "_initCallback"
  }


  private def call(fnc:String,args: Array[Object]=null) : Object = {
    if (true) println("window: "+_window+"  call: "+fnc+" args: "+args)
    if (_window!=null) _window.call(fnc, args) else null
  }

  private def setTimeout(message:Array[Object]) : Object = {
    call("setTimeout", message)
  }

  private def callAndForget(func:String, args:String="") : Object = {
    val message = Array[Object] (
      _subPrefix + _apiPrefix + func + "( " + args + ")",
      new java.lang.Integer(0)
    )
    setTimeout(message)
  }

  private def buttonStateCallback(attr:String, state:Boolean) = {
    callAndForget("_buttonStateCallback", "'" + attr + "'," +
                  (if (state) "true" else "false"))
  }

  private def graphImportedCallback(msg:String) = {
    callAndForget("_graphImportedCallback", msg)
  }


  def act() {

    //var model = new Model()

    loop {
      react {
        case func:String => callAndForget(func)
        case (func:String,args:String) => callAndForget(func,args)
        case msg => log("unknow msg: "+msg)
      }
    }
  }



}
