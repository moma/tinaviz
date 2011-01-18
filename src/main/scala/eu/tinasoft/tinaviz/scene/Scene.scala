/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.scene

import eu.tinasoft.tinaviz._

import util.Color
import util.Color._

/**
 * An immutable, straightforward data-structure used for final rendering to screen
 */
case class Scene(
                  val background: Color = new Color(0.0, 0.0, 1.0),
                  val foreground: Color = new Color(0.0, 1.0, 0.0),
                  val labelColor: Color = new Color(0.0, 1.0, 0.0),

                  val nbNodes: Int = 0,
                  val nbEdges: Int = 0,

                  // nodes
                  val nodePositionLayer: Array[(Double, Double)] = Array.empty[(Double, Double)],
                  val nodeColorLayer: Array[Color] = Array.empty[Color],
                  val nodeBorderColorLayer: Array[Color] = Array.empty[Color],
                  val nodeShapeLayer: Array[Symbol] = Array.empty[Symbol],
                  val nodeSizeLayer: Array[Double] = Array.empty[Double],

                  val nodeLabelLayer: Array[String] = Array.empty[String],

                  // edges
                  val edgePositionLayer: Array[((Double, Double),
                    (Double, Double))] = Array.empty[((Double, Double),
                    (Double, Double))],
                  val edgeColorLayer: Array[Color] = Array.empty[Color],
                  val edgeWeightLayer: Array[Double] = Array.empty[Double]


                  ) {

}

