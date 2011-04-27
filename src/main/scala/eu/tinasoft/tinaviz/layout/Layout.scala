/**
 * Copyright (C) 2009-2011 CREA Lab, CNRS/Ecole Polytechnique UMR 7656 (Fr)
 */

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
                       case g:Graph =>
                         //println("Layout: got graph ("+g.nbNodes+")")
                         g
                       case any =>
                         //println("Layout: empty graph..")
                         new Graph
                   })
                ).toGraph )

              this ! 'run  // run layout as fast as possible
          case 'exit =>
              exit()
        }
    }
  }


}
