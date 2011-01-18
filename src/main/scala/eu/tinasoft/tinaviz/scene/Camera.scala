/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.scene

/**
 * Main Camera class
 *
 * some attributes are outdated, and should be removed (eg. some default zooms
 * values are not used)
 */
class Camera(

              var position: (Double, Double) = (150, 150),
              var positionDelta: (Double, Double) = (0, 0),

              var zoom: Double = 1.15,
              var zoomDelta: Double = 0,

              // CAMERA CENTER
              var center: (Double, Double) = (0, 0),

              // LAST POSITION OF THE MOUSE
              var lastMousePosition: (Double, Double) = (0, 0),


              // DEFAULT ZOOM RATIOS
              /*
         var defaultZoomRatio : Double = 0.3,
         var doubleZoomRatio : Double = 0.43,
         var defaultZoomInRatio : Double = 1.3,
         var defaultDoubleZoomInRatio : Double = 1.43,
         var defaultZoomOutRatio : Double = 0.76,
         var defaultDoubleZoomOutRatio : Double = 0.63,*/


              // IS THE CAMERA BEING MOUSE-DRAGGED?
              var dragged: Boolean = false
              ) {

  // IS THE CAMERA BEING MOVED? (MOUSE DRAG AND/OR ALREADY BEING ZOOMING)
  def isMoving: Boolean = {
    (zoomDelta != 1.0) || dragged
  }

}