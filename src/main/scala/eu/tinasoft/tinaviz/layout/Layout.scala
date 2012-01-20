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

package eu.tinasoft.tinaviz.layout

import eu.tinasoft._
import tinaviz.graph.Graph
import tinaviz.pipeline.Pipeline
import tinaviz.pipeline.Workflow

import actors.Actor
import actors.Actor._
import tinaviz.{Session, Main, Server}

/**
 * Layout Actor
 */
class Layout (val session:Session) extends Actor {

  def now() = {
    System.currentTimeMillis
  }

  def act() {
    val layoutEngine = new PhysicLayout(session)
    var running = false
     self ! 'start

   // loop {
   //   react {
    while (true) {
     receive {
        case 'start  =>
          if (!running) {
            //println(" Layout: asked to start, and not already running, so..starting!")
          running = true
          self ! 'run
          }

        case 'stop  =>
          if (running) {
           // println(" Layout: running, so stopping")
            running = false
          }

        case 'run =>
          if (running) {
           // println("running layout")
            val start = now()

            val g = session.pipeline.output
            //println("layouting the output")
            val h = layoutEngine.layout(g)

            val computingTime = now() - start

            actor {
              var waitTime = if (computingTime < (1000.0 / g.layoutSpeed)) ((1000.0 / g.layoutSpeed) - computingTime) else 0.0
              Thread.sleep( waitTime.toInt )
              session.workflow ! 'overwriteCoordinates -> h
            }
          }

        case 'exit =>
          exit()
      }
    }
  }
}
