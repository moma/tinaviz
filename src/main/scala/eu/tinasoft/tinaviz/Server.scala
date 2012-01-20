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

import eu.tinasoft._

import tinaviz.io._
import tinaviz.graph._
import tinaviz.scene._
import tinaviz.pipeline._
import tinaviz.layout._

import actors._
import Actor._
import xml.dtd.ValidationException

case class Step(val step: Symbol)

/**
 * This class need a big refactoring..
 */
class Server(val session: Session) extends Actor {

  val defaultProperties: Map[String, Any] = Map(
    // global real FPS
    "frameRate" -> 0,
    "filter.selection" -> List.empty[String],

    // current view settings
    "filter.view" -> "macro",
    "filter.node.category" -> "Document",
    "filter.a.node.weight" ->(0.0, 1.0),
    "filter.a.edge.weight" ->(0.0, 1.0),
    "filter.a.node.size" -> .2,
    "filter.b.node.weight" ->(0.0, 1.0),
    "filter.b.edge.weight" ->(0.0, 1.0),
    "filter.b.node.size" -> .2,
    "pause" -> false,
    "debug" -> false,

    // global selection disk settings
    "selectionRadius" -> 10.0,

    // TODO real-time camera settings
    "camera.zoom" -> 0.5,
    "camera.position" ->(0.0, 0.0),
    "camera.target" -> "all",


    "screen.width" -> 100,
    "screen.height" -> 100,

    "input" -> new Graph()
  )

  var properties: Map[String, Any] = defaultProperties

  def act() {

    while (true) {
      receive {

        case 'exit =>
          println("Server: calling exit() on myself")
          exit()

        case ("camera.mouse", kind, side, count, position) =>
          session.workflow !("camera.mouse", kind, side, count, position)

        case ('updated, key: String, value: Any, previous: Any) =>
          //println("Server: ('updated, "+key+", "+value+", "+previous+") --")

          key match {
            case "frameRate" =>
            //case "selectionRadius" =>
            //println("Session: catched selectionRadius: "+value)
            //session.pipeline.applyKey("selectionRadius",value)

            case any =>
              //println(" '--> some value have been updated manually: workflow ! "+key+" -> "+value)
              session.workflow ! key -> value
          }
        // async get

        case (cb: Int, something) =>
          something match {

            case ('open, pathOrURL: Any) =>
              //println("'open "+pathOrURL)
              session.webpage ! cb -> Map("status" -> "downloading")

              (new GEXF(session)) ! cb -> (pathOrURL, properties)

            case 'graphDownloaded =>
              //println("'graphDownloaded")
              session.webpage ! cb -> Map("status" -> "downloaded")

            case g: Graph =>
              // println("Workflow: graph Stream, after the callback: "+g.nbNodes+" nodes and "+g.nbEdges+" edges")
              //println("WORKFLOW: g: "+g.uuid.size)
              //println("Server: Graph: " + g.uuid.size)
              session.pipeline.setInput(g)

              //println("Server: IMPORTANT propagating category filter\n\n")
              //session.server ! "filter.view" -> g.currentView // TODO DEBUG maybe this is useless

              // workflow won't cache
              session.workflow ! "filter.node.category" -> g.currentCategory

              // maybe this one is optional?
              session.server ! "filter.node.category" -> g.currentCategory

              //println("Server: sending message to web client")
              session.webpage ! cb -> Map("status" -> "updated")


            case 'graphLoaded =>
              //println("Server: graph loaded. sending message to client")
              session.webpage ! cb -> Map("status" -> "loaded")

            case ('updated, key: String, value: Any, previous: Any) =>
              session.workflow ! cb ->(key, value)

            case (key: String, value: Any) => setSome(cb, key, value)
            case (key: String) =>
              //println("server: javascript asked for a global property: " + key + ", answering with async map to " + cb)
              session.webpage ! cb -> Map(key -> properties(key))
            case unknow =>
              //println("server: received unknow cmd " + unknow + ", forwarding")
              session.workflow ! cb -> unknow
          }


        case key: String =>

          reply(properties(key))

        case (key: String, value: Any) =>
          //println("Server: received "+key+" -> "+value)
          setSome(-1, key, value)

        case msg => println("Server: error, unknow msg: " + msg)
      }
    }

  }

  def setSome(cb: Int, key: String, pvalue: Any) = {

    var value = pvalue
    if (!properties.contains(key)) {
      properties += key -> value
      if (cb != -1) {
        self ! cb -> ('updated, key, value, value)
      }  else {
        self ! ('updated, key, value, value)
      }
    } else {
      val previous = properties(key)
      value = value match {
        // special case for booleans
        case 'toggle =>
          previous match {
            case b: Boolean =>
              !b
            case s: String =>
              s match {
                case "Document" => "NGram"
                case "NGram" => "Document"
                case "macro" => "meso"
                case "meso" => "macro"
                case any => any
              }
            case x => x
          }
        // default case
        case x => value
      }
      properties += key -> value
      //
      //reply(previous)
      if (!previous.equals(value)) {
        if (cb != -1) {
          self ! cb -> ('updated, key, value, previous)
        } else {
          self ! ('updated, key, value, previous)
        }
      }
    }
  }

  def get[T](key: String): T = properties.get(key).get.asInstanceOf[T]
}
