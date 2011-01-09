/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import org.daizoru._
import eu.tinasoft._

import tinaviz.graph._
import Sketch._

import actors._
import Actor._

/**
 * todo: use caching to detect changes, and only update the sketch for these
 */
class Sketcher extends node.util.Actor {
  start
  def act() {

    var cache = new Graph()
    val sketch = new Sketch()

    while (true) {
      receive {

        case ('position, graph:Graph) =>
          println("Sketcher: updating positions..")
          cache = cache
          sketch.updateNodePositions(graph)
          reply(sketch:Scene)

        case graph:Graph =>
          println("Sketcher: overwriting..")
          cache = graph
          sketch.overwrite(graph)
          reply(sketch:Scene)
      }
    }
  }
}
