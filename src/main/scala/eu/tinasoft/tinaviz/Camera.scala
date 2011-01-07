/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

class Camera(
  var cameraDelta : (Double, Double) = (0, 0),
  var defaultZoomRatio : Double = 0.3,
  var doubleZoomRatio : Double = 0.43,
  var defaultZoomInRatio : Double = 1.3,
  var defaultDoubleZoomInRatio : Double = 1.43,
  var defaultZoomOutRatio : Double = 0.76,
  var defaultDoubleZoomOutRatio : Double = 0.63,
  var zRatio : Double = 1.0,
  var lastMousePosition : (Double,Double) = (0, 0),
  var dragged : Boolean = false,
  var center : (Double,Double) = (0,0)) {

  def isMoving : Boolean = {
    zRatio != 1.0 || dragged
  }

}