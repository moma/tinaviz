/************************************************************************
                                  Tinaviz
*************************************************************************
 This application is part of the Tinasoft project: http://tinasoft.eu
 Tinaviz main developer: julian.bilcke @ iscpif.fr  (twitter.com/flngr)

 Copyright (C) 2009-2011 CREA Lab, CNRS/Ecole Polytechnique UMR 7656 (Fr)

 This program is free software: you can redistribute it and/or modify it
 under the terms of the GNU General Public License as published by the
 Free Software Foundation, either version 3 of the License, or (at your
 option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program. If not, see <http://www.gnu.org/licenses/>.
************************************************************************/

package eu.tinasoft.tinaviz.scene

import processing.core._

import eu.tinasoft._
import tinaviz.util._
import tinaviz.util.Vector._
import java.awt.event.MouseWheelListener
import java.awt.event.MouseWheelEvent

import tinaviz.{Session, Server}
import traer.physics._

class Fonts(val p: PApplet,
            val fontName: String = "Arial",
            val size: Int = 100,
            val defaultFontSize: Int = 12,
            val min: Int = 8) {

  val fonts = for (i <- List.range(1, size+1)) yield p.createFont(fontName, i, true)
  val boldFonts = for (i <- List.range(1, size+1)) yield p.createFont(fontName+" Bold", i, true)

  val defaultFont = fonts(defaultFontSize)

  def get(s: Int, bold: Boolean) = {
    val sz = if (s < min) min else (if (s >= size) (size - 1) else s)
    ((if (bold) boldFonts(sz) else fonts(sz)), sz)
  }

}

/*
class PDFFonts(val p: PApplet,
            val fontName: String = PGraphicsPDF.list()(0),//"Arial",
            val size: Int = 100,
            val defaultFontSize: Int = 12,
            val min: Int = 8) {

  val fonts = for (i <- List.range(1, size+1)) yield p.createFont(fontName, i, true)
  val boldFonts = for (i <- List.range(1, size+1)) yield p.createFont(fontName, i, true)

  val defaultFont = fonts(defaultFontSize)

  def get(s: Int, bold: Boolean) = {
    val sz = if (s < min) min else (if (s >= size) (size - 1) else s)
    ((if (bold) boldFonts(sz) else fonts(sz)), sz)
  }

}
*/

/**
 * Main class
 *
 * TApplet inherits from PApplet, the Processing way of doing applets. I didn't
 * reinvented the wheel here. I just added some additional functions, altough
 * not perfect yet : I created new functions names, but it could be done by
 * override each single function, to be more elegant.
 *
 * @param
 * @return
 * @throws
 */
class TApplet extends PApplet with MouseWheelListener {


  private var session : Session = null
  def setTAppletSession(s:Session) = { session = s }

  // particle system for the Camera (smooth moves)
  //
  val ps = new ParticleSystem(0f, 0.1f)

  val psRoof =   ps.makeParticle( 1.0f,    0.00f,   0.0f,   0.0f)
  psRoof.makeFixed() // don't touch this
  val psCamera = ps.makeParticle( 1.0f,    0.05f,   0.0f,   0.0f)
  val psSpring = ps.makeSpring(psRoof, psCamera, 0.01f,  0.01f,  0.05f )

  val minZoom = 0.015 // old value was 0.050
  val maxZoom = 500.0

  private val _fonts = new Fonts(this)
  private val _camera = new Camera()

  def getZoom = _camera.zoom

  def getPosition = _camera.position

  def getCamera = (_camera.position, _camera.zoom)

  /**
   * Draw a square-like shape from a Double Tuple, which represents the position.
   * You must also give a side length.
   *
   */
  protected def drawSquare(position: (Double, Double), side: Double) = {
    rect(position._1.toFloat, position._2.toFloat, side.toFloat, side.toFloat)
  }

  /**
   * Draw a disk-like shape from a Double Tuple, which represents the position.
   * You must also give a radius.
   *
   */
  protected def drawDisk(position: (Double, Double), radius: Double) = {
    ellipse(position._1.toFloat, position._2.toFloat, radius.toFloat, radius.toFloat)
  }
  /**
   * Draw a lane-like shape from two Double Tuples, which represent the source
   * and target positions.
   *
   */
  protected def drawLine(n1: (Double, Double), n2: (Double, Double)) = {
    line(n1._1.toFloat, n1._2.toFloat, n2._1.toFloat, n2._2.toFloat)
  }

  /**
   * Draw a curve-like shape from two Double Tuples, which represent the source
   * and target positions.
   *
   */
  protected def drawCurve(n1: (Double, Double), n2: (Double, Double)) = {
    drawCurve4(n1._1.toFloat, n1._2.toFloat, n2._1.toFloat, n2._2.toFloat)
  }

  /**
   * Draw a Bezier curve from two Double Tuples, which represent the source
   * and target positions. The curve "height" is fixed.
   *
   * Original code is not from me, but the original prototype of graphviz
   *
   */
  private def drawCurve4(n1x: Float, n1y: Float, n2x: Float, n2y: Float) = {

    val xa0 = (6 * n1x + n2x) / 7
    val ya0 = (6 * n1y + n2y) / 7
    val xb0 = (n1x + 6 * n2x) / 7
    val yb0 = (n1y + 6 * n2y) / 7

    val xya1a = (n1x + (xa0 - n1x) * Maths.COS_PI_ON_TWO - (ya0 - n1y) * Maths.SIN_PI_ON_TWO)
    val xya1b = (n1y + (xa0 - n1x) * Maths.SIN_PI_ON_TWO + (ya0 - n1y) * Maths.COS_PI_ON_TWO)
    val xyb1a = (n2x + (xb0 - n2x) * Maths.COS_MINUS_PI_ON_TWO - (yb0 - n2y) * Maths.SIN_MINUS_PI_ON_TWO)
    val xyb1b = (n2y + (xb0 - n2x) * Maths.SIN_MINUS_PI_ON_TWO + (yb0 - n2y) * Maths.COS_MINUS_PI_ON_TWO)

    //if (curveMode == curveMode.CURVY) {
    bezier(n1x, n1y,
      xya1a.toFloat, xya1b.toFloat,
      xyb1a.toFloat, xyb1b.toFloat,
      n2x, n2y)
    //} else {
    //    line(n1x, n1y, n2x, n2y);
    //}
  }


  /**
   * Convert a Tuple2 of Double to a PVector
   */
  implicit def tuple2ToPVector(v: (Double, Double)): PVector = {
    new PVector(v._1.toFloat, v._2.toFloat, 0.0f)
  }

  /**
   * Convert a Tuple2 of Double to a PVector
   */
  implicit def tuple2iToPVector(v: (Int, Int)): PVector = {
    new PVector(v._1.toFloat, v._2.toFloat, 0.0f)
  }

  /**
   * Convert a Tuple3 of Double to a PVector
   */
  implicit def tuple3ToPVector(v: (Double, Double, Double)): PVector = {
    new PVector(v._1.toFloat, v._2.toFloat, v._3.toFloat)
  }

  /**
   * Convert a PVector to a Tuple2 of Double
   */
  implicit def pvectorToTuple2(v: PVector): (Double, Double) = {
    (v.x.toDouble, v.y.toDouble)
  }

  /**
   * Convert a PVector to a Tuple2 of Double
   */
  implicit def pvectorToTuple2i(v: PVector): (Int, Int) = {
    (v.x.toInt, v.y.toInt)
  }

  /**
   * Convert a PVector to a Tuple3 of Double
   */
  implicit def pvectorToTuple3(v: PVector): (Double, Double, Double) = {
    (v.x.toDouble, v.y.toDouble, v.z.toDouble)
  }

  /**
   * Move the Camera at a given position
   */
  protected def moveCameraAt(x: Double, y: Double, z: Double) {
    translate(x.toFloat, y.toFloat)
    scale(z.toFloat)
  }

  protected def setupCamera {
    // todo use the smoother
    moveCameraAt(_camera.position._1, _camera.position._2, _camera.zoom)
  }

  /**
   * Set the Background color
   */
  protected def setBackground(c: Color) = {
    background(c.h.toFloat, c.s.toFloat, c.b.toFloat, c.a.toFloat)
  }

  protected def setFontSize(size: Int, bold: Boolean) : Int = {
    val fsz = _fonts.get(size.toInt, bold)
    textFont(fsz._1)
    fsz._2
  }
  protected def tryFontSize(size: Int, bold: Boolean) : Int = {
    val fsz = _fonts.get(size.toInt, bold)
    //textFont(fsz._1)
    fsz._2
  }
  protected def setColor(c: Color) = {
    fill(c.h.toFloat, c.s.toFloat, c.b.toFloat, c.a.toFloat)
  }

  protected def setLod(v: Int) = {
    bezierDetail(v)
  }

  protected def lineColor(c: Color) = {
    stroke(c.h.toFloat, c.s.toFloat, c.b.toFloat, c.a.toFloat)
  }

  protected def lineThickness(t: Double) = {
    strokeWeight(t.toFloat)
  }

  protected def distance(a: (Int, Int), b: (Int, Int)): Double = {
    PApplet.dist(a._1.toFloat, a._2.toFloat, b._1.toFloat, b._2.toFloat)
  }


  def limit(v: Double, min: Double, max: Double): Double = {
    if (v < min) min else if (v > max) max else v
  }

  protected def zoomUpdated(v: Double) {}

  protected def positionUpdated(v: (Double, Double)) {}

  protected def mouseUpdated(kind: Symbol,
                             side: Symbol,
                             count: Symbol,
                             position: (Double, Double)) {}

  /**
   * Zoom to the screen's center
   */
  def zoom(zoomIn: Boolean) {
    zoomWith(if (zoomIn) 1.3 else 0.7)
  }

  /**
   * Zoom to the screen's center
   */
  def zoomWith(zoomRatio:Double) {
    _camera.lastMousePosition = (mouseX, mouseY)

    val z = limit(_camera.zoom * zoomRatio, minZoom, maxZoom)
    if (z != minZoom && z != maxZoom) {
        updateZoom(z)
        val p: PVector = _camera.position
        val center = (mouseX, mouseY)
        p.sub(center)
        p.mult(zoomRatio.toFloat)
        p.add(center)
        updatePosition(p)
    }
  }
  def updateZoomSilent(value: Double) {
    _camera.zoom = value
  }

  def updateZoom(value: Double) {
    _camera.zoom = value
    zoomUpdated(value)
  }

  def updatePositionSilent(value: (Double, Double)) {
    _camera.position = value
  }

  def updatePosition(value: (Double, Double)) {
    _camera.position = value
    positionUpdated(value)
  }

  def updateMouse(kind: Symbol) {
    //zoomUpdated(_camera.zoom)
    _camera.lastMousePosition = mouseXY
    mouseUpdated(kind, whichButton, clickCount, mouseXY)
  }

  /**
   * Move the Camera and zoom to a specific position
   */
  def moveCameraTo(position: (Double, Double), zoomLevel: Double = 5.0) {
    _camera.lastMousePosition = (mouseX, mouseY)
    updateZoom(limit(zoomLevel, minZoom, maxZoom))
    updatePosition(position)
  }



  /**
   * Are the given coordinate invisible?
   */
  def isVisible(p: (Int, Int)) = {
    val w = width / 48 // changed from 1/16 to 1/32
    val h = height / 128 // in height, we don't care too much
    ((p._1 > -w) && (p._1 < (width + w))
      && (p._2 > -h) && (p._2 < (height + h)))
  }

  /**
   * Are the given coordinate visible?
   */
  def isInvisible(p: (Int, Int)) = !isVisible(p)

  /**
   * TODO could be optimized, by using the reverse action (translate, zoom)
   * Thus we could use this function anywhere, if we have access to camera value
   */
  def screenPosition(p: (Double, Double)): (Int, Int) = screenPosition(p._1, p._2)


  /**
   * TODO could be optimized, by using the reverse action (translate, zoom)
   * Thus we could use this function anywhere, if we have access to camera value
   */
  def screenPosition(x: Double, y: Double): (Int, Int) = (screenX(x.toFloat, y.toFloat).toInt,
                                                          screenY(x.toFloat, y.toFloat).toInt)

  //def modelPosition(p:(Int,Int)) : (Double,Double) = modelPosition(p._1,p._2)
  //def modelPosition(x:Int,y:Int) : (Double,Double) = modelPosition(x,y)
  def modelPosition(p:(Double,Double)) : (Double,Double) = modelPosition(p._1,p._2)
  def modelPosition(x:Double,y:Double) : (Double,Double) = (
    (x.toDouble / _camera.zoom) - _camera.position._1,
    (y.toDouble / _camera.zoom) - _camera.position._2)

  /**
   * Get the size to the screen
   */
  def screenSize(s: Double): Int = {
    (s * _camera.zoom).toInt
  }


  def showSelectionCircle(radius: Double) {
    //println("showSelectionCircle: "+radius)
    if (radius < 1) return


    stroke(0.0f, 1.0f, 0.0f, 0.5f)
    strokeWeight(1.0f)
    fill(.57f, 1.0f, 1.0f, 0.2f)
    drawDisk(mouseXY, radius)


  }

  def goToScreen() {
    scale((1.0 / _camera.zoom).toFloat)
    translate(-_camera.position._1.toFloat, -_camera.position._2.toFloat)

  }
  def goToScene() {

     translate(_camera.position._1.toFloat, _camera.position._2.toFloat)
     scale(_camera.zoom.toFloat)

   }


  def mouseXY = (mouseX.toDouble, mouseY.toDouble)

  def mouseXYInModel = {
    ((mouseX.toDouble / _camera.zoom) - _camera.position._1,
      (mouseY.toDouble / _camera.zoom) - _camera.position._2)
  }


  def stopAutoCentering {

  }
  def setCameraPosition(x:Int,y:Int) {
    _camera.position = (x,y)
  }
  override def mouseDragged {
    stopAutoCentering
    val p: PVector = _camera.position
    val t: PVector = _camera.position
    t.sub(_camera.lastMousePosition)
    _camera.lastMousePosition = mouseXY
    t.add(_camera.lastMousePosition)
    _camera.positionDelta = PVector.sub(p, t)
    _camera.dragged = true
    updatePosition(t)
    updateMouse('Drag)
    session.server ! "camera.target" -> "none"
  }

  override def mouseClicked = {
    if ((mouseY > height - 28) && (mouseX < 25)) {
      link("http://sciencemapping.com", "_new")
    } else {
       updateMouse('Click)
    }

  }

  override def mouseMoved = updateMouse('Move)

  override def mouseReleased {
    _camera.dragged = false
    updateMouse('Release)
  }

  // mouseUpdated(('Click, whichButton, clickType), mouseXY, mouseXYInModel)
  //

  override def mouseWheelMoved(e: MouseWheelEvent) {
    if (!(mouseX < 0
      | mouseX > width
      | mouseY < 0
      | mouseY > height)) {
      if (e.getUnitsToScroll != 0) {
        zoom(e.getWheelRotation < 0)
      }
    }
    resetIdle
    session.server ! "camera.target" -> "none"
  }

  // detection system for user idle
  var _idle = 0
  def idle = _idle
  def resetIdle : Int = {
    _idle = 0
    0
  }
  def increaseIdle : Int = {
    _idle = idle + 1
    _idle
  }

  def moveUp(amount: Double = 10) = _camera.position = (_camera.position._1, _camera.position._2 + amount)

  def moveDown(amount: Double = 10) = _camera.position = (_camera.position._1, _camera.position._2 - amount)

  def moveLeft(amount: Double = 10) = _camera.position = (_camera.position._1 + amount, _camera.position._2)

  def moveRight(amount: Double = 10) = _camera.position = (_camera.position._1 - amount, _camera.position._2)


  /**
   * Return which button is clicked ('Left, 'Right or 'Middle)
   */
  def whichButton = mouseButton match {
    case PConstants.LEFT => 'Left
    case PConstants.RIGHT => 'Right
    case any => 'Middle
  }

  /**
   * Return the click count, as a symbol ('Simple, or 'Double)
   */
  def clickCount = if (mouseEvent.getClickCount() == 2) 'Double else 'Simple

}