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
  
  val maxLod : Int = 120
  /*
  val maxLod : Int = nbEdges match { 
    case i:Int => 
        if (i < 2000) {
          120
        } else if (i < 4000) {
          100
        } else if (i < 6000) {
          80
        } else if (i < 8000) {
          60
        } else if (i < 10000) {
          40
        } else if (i < 12000) {
          30
        } else if (i < 14000) {
          25
        } else if (i < 16000) {
          20
        } else if (i < 18000) {
          15
        } else if (i < 20000) {
          10
        } else if (i < 22000) {
          8
        } else if (i < 24000) {
          6
        } else if (i < 26000) {
          4
        } else if (i < 28000) {
          3
        } else if (i < 30000) {
          2
        } else {
          1
        }
    }
 */
}

