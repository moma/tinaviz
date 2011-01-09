/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.sketch

import org.daizoru._
import eu.tinasoft._

import tinaviz.graph.Graph
import tinaviz.scene.Scene

import tinaviz.sketch.Sketch._

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
          cache = graph
          sketch.updateNodePositions(graph)
          // println("  Renderer: done partial compilation of scene (position layer)..")
          reply(sketch:Scene)

        case graph:Graph =>

          cache = graph
          sketch.overwrite(graph)
          // println("  Renderer: done complete compilation of scene..")
          reply(sketch:Scene)
      }
    }
  }
}
