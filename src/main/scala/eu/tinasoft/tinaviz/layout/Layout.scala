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

import tinaviz.Main
import tinaviz.Server

import actors.Actor
import actors.Actor._

/**
 * Layout Actor
 */
object Layout extends Actor {

  def act() {

    this ! 'run

    while (true) {
      receive {
        case 'run =>
          Workflow ! ('setLayout,
            PhysicLayout.layout(
              ((Workflow !? 'getLayout) match {
                case g: Graph =>
                //println("Layout: got graph ("+g.nbNodes+")")
                  g
                case any =>
                //println("Layout: empty graph..")
                  new Graph
              })
            ).warmCache) // warm the cache, so the visualization thread don't have to do it

          this ! 'run // run layout as fast as possible
        case 'exit =>
          exit()
      }
    }
  }
}
