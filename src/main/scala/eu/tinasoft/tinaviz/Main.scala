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

package eu.tinasoft.tinaviz

import graph.Filters
import io.GEXF
import layout.PhysicLayout

object Main {


  /**
   * main method
   */
  def main(args: Array[String]): Unit = {

    val graph = GEXF.open("<xml></xml>")
    val iterations = 100

    println("computing social graph layout")
    val socialGraph = Filters.category(graph, "document")
    var i = 0
    var newSocialGraph = socialGraph
    val socialLayout = new PhysicLayout()
    while (i < iterations) {
      i += 1
      newSocialGraph = socialLayout.layout(newSocialGraph)
    }

    println("computing semantic graph layout")
    val semanticGraph = Filters.category(graph, "semantic")
    var j = 0
    var newSemanticGraph = semanticGraph
    val semanticLayout = new PhysicLayout()
    while (j < iterations) {
      j += 1
      newSemanticGraph = semanticLayout.layout(newSemanticGraph)
    }

    // merge coordinates
    println("merging coordinates from both graphs")
    var finalGraph = graph

    val newPositions = graph.position.zipWithIndex.map {
      case (elem, index) => {
        val uuid = graph.getUuid(index)
        val idInSemanticGraph = semanticGraph.id(uuid)
        val idInSocialGraph = socialGraph.id(uuid)
        if (idInSemanticGraph > -1) {
          semanticGraph.position(idInSemanticGraph)
        }
        else if (idInSocialGraph > -1) {
          socialGraph.position(idInSocialGraph)
        } else {
          elem
        }
      }
    }
    val finalGraph = graph + ( "position" -> newPositions)

    val newGexf = GEXF.export(finalGraph)
  }

}
