/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz


case class Sketch (
  var background : Color =  new Color(255,255,255),
  var foreground : Color =  new Color(0,0,0),
  var labelColor : Color = new Color (0,0,0),

  // nodes
  var nodePositionLayer : Array[(Double,Double)] = Array.empty[(Double,Double)],
  var nodeColorLayer : Array[Color] = Array.empty[Color],
  var nodeShapeLayer : Array[Symbol] = Array.empty[Symbol],
  var nodeSizeLayer : Array[Double] = Array.empty[Double],

  // edges
  var edgePositionLayer : Array[((Double,Double),
                                 (Double,Double))] = Array.empty[((Double,Double),
                                                                  (Double,Double))],
  var edgeColorLayer : Array[Color] = Array.empty[Color],
  var edgeWeightLayer : Array[Double] = Array.empty[Double],


  // labels
  var labelPosition : Array[(Double,Double)] = Array.empty[(Double,Double)],
  var labelTextLayer : Array[String] = Array.empty[String],
  var labelColorLayer : Array[Color] = Array.empty[Color]
)  {
  
  def toScene = new Scene(
    background,
    foreground,
    labelColor,

    // nodes
    nodePositionLayer,
    nodeColorLayer,
    nodeShapeLayer,
    nodeSizeLayer,

    // edges
    edgePositionLayer,
    edgeColorLayer,
    edgeWeightLayer,


    // labels
    labelPosition,
    labelTextLayer,
    labelColorLayer
  )

}
