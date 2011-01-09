/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import Color._


case class Scene (
  val background : Color =  new Color(255,255,255),
  val foreground : Color =  new Color(0,0,0),
  val labelColor : Color = new Color (0,0,0),

  // nodes
  val nodePositionLayer : Array[(Double,Double)] = Array.empty[(Double,Double)],
  val nodeColorLayer : Array[Color] = Array.empty[Color],
  val nodeShapeLayer : Array[Symbol] = Array.empty[Symbol],
  val nodeSizeLayer : Array[Double] = Array.empty[Double],

  // edges
  val edgePositionLayer : Array[((Double,Double),
                                 (Double,Double))] = Array.empty[((Double,Double),
                                                                  (Double,Double))],
  val edgeColorLayer : Array[Color] = Array.empty[Color],
  val edgeWeightLayer : Array[Double] = Array.empty[Double],


  // labels
  val labelPosition : Array[(Double,Double)] = Array.empty[(Double,Double)],
  val labelTextLayer : Array[String] = Array.empty[String],
  val labelColorLayer : Array[Color] = Array.empty[Color]
) {
  
}

