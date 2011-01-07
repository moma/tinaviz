/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

/**
 * Main Camera class
 * 
 * some attributes are outdated, and should be removed (eg. some default zooms
 * values are not used)
 */
class Camera (
  
  // DEFAULT CAMERA POSITION
  var position : (Double,Double,Double) = (0,0,0),
  
  // CAMERA CENTER
  var center : (Double,Double) = (0,0),
    
  // LAST POSITION OF THE MOUSE
  var lastMousePosition : (Double,Double) = (0, 0),
  
  // CAMERA DELTA (FOR SMOOTHING)
  var cameraDelta : (Double, Double) = (0, 0),
  
  // DEFAULT ZOOM RATIOS
  var defaultZoomRatio : Double = 0.3,
  var doubleZoomRatio : Double = 0.43,
  var defaultZoomInRatio : Double = 1.3,
  var defaultDoubleZoomInRatio : Double = 1.43,
  var defaultZoomOutRatio : Double = 0.76,
  var defaultDoubleZoomOutRatio : Double = 0.63,
  
  // CURRENT ZOOM RATIO
  var zRatio : Double = 1.0,
  
  // IS THE CAMERA BEING MOUSE-DRAGGED?
  var dragged : Boolean = false
) {

  // IS THE CAMERA BEING MOVED? (MOUSE DRAG AND/OR ALREADY BEING ZOOMING)
  def isMoving : Boolean = {
    zRatio != 1.0 || dragged
  }

}