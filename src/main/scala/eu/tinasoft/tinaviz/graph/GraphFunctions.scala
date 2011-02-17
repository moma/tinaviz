package eu.tinasoft.tinaviz.graph

/**
 * Created by IntelliJ IDEA.
 * User: jbilcke
 * Date: 2/17/11
 * Time: 12:33 PM
 * To change this template use File | Settings | File Templates.
 */

object GraphFunctions  {
  /**
   * Return a new graph which is centered
   * we can center it using XY normalization, or camera, or camera force.. for the moment we keep it simple
   */
  def recenter (g:Graph) = {
    //var newCameraPosition = get[(Double,Double)]("camera.position")
    // assuming the graph is centered in 0.0
     g + ("camera.position" -> (0.0,0.0))
  }

}