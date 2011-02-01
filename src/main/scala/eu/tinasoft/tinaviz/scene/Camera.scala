/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.scene

/**
 * Main Camera class
 *
 * This is one of my first classes.. should be rewritten to be immutable
 *
 * some attributes are obsolete and should be removed (eg. some default zooms values are not used)
 */
class Camera(

              var position: (Double, Double) = (150, 150),
              var positionDelta: (Double, Double) = (0, 0),

              var zoom: Double = 1.15,
              var zoomDelta: Double = 0,

              // LAST POSITION OF THE MOUSE
              var lastMousePosition: (Double, Double) = (0, 0),

              // Camera Animation
              var animation : Symbol = 'None,

              var positionAnim : (Double, Double) = (0,0),
              var zoomAnim : Double = 0,

              // IS THE CAMERA BEING MOUSE-DRAGGED?
              var dragged: Boolean = false
              ) {

  // IS THE CAMERA BEING MOVED? (MOUSE DRAG AND/OR ALREADY BEING ZOOMING)
  def isMoving: Boolean = {
    (zoomDelta != 1.0) || dragged
  }

}