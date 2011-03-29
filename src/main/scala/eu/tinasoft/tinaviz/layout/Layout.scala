package eu.tinasoft.tinaviz.layout

/**
 * Created by IntelliJ IDEA.
 * User: jbilcke
 * Date: 3/29/11
 * Time: 11:28 AM
 * To change this template use File | Settings | File Templates.
 */

import org.daizoru._
import eu.tinasoft._
import tinaviz.graph.Graph
import tinaviz.pipeline.Pipeline

import tinaviz.Main
import tinaviz.Server
/**
 * Layout Actor
 */
object Layout extends node.util.Actor {

  def act() {

      this ! 'run

      while (true) {
        receive {
          case 'run =>
              println("Layout: running")
              // take current output (viewed graph), run a layout, then evaluate it to "warm up" the lazy's cache
              Pipeline.setOutput(
                    PhysicLayout.layout(Pipeline.output).toGraph
              )
              this ! 'run  // run layout as fast as possible
          case 'exit =>
              exit()
        }
    }
  }


}
