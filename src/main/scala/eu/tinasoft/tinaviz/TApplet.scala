/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

import java.awt.event.MouseWheelEvent
import java.awt.event.MouseWheelListener
import processing.core._

class Fonts(val p : PApplet,
            val fontName : String = "Arial",
            val size:Int=80,
            val defaultFontSize : Int = 12) {

  val fonts = for (i <- List.range(1,size))
    yield p.createFont(fontName, i, true)

  val defaultFont = fonts(defaultFontSize)   
    
  def get(s:Int) = fonts (if (s > 1) (if (s < size) s else size) else 1)
}


class TApplet extends PApplet with MouseWheelListener {
  
  private val _fonts = new Fonts(this)
  private val _camera = new Camera()
  
  protected def drawSquare(position:(Double,Double),radius:Double) = {
    rect(position._1.toFloat,position._2.toFloat,radius.toFloat,radius.toFloat)
  }
  protected def drawDisk(position:(Double,Double),radius:Double) = {
    ellipse(position._1.toFloat,position._2.toFloat,radius.toFloat,radius.toFloat)
  }

  protected def drawCurve(n1:(Double,Double), n2:(Double,Double)) = {
    drawCurve4(n1._1.toFloat,n1._2.toFloat,n2._1.toFloat,n2._2.toFloat)
  }

  private def drawCurve4(n1x:Float, n1y:Float, n2x:Float, n2y:Float) = {

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



  /*
   private def tryZoom(v:Double) : Boolean = {
   val futureValue = ratio
   if ((v >= CEIL) && (v <= FLOOR)) {
   //sceneScale = newValue;
   return true;
   }
   if (((v <= CEIL) && (v >= sceneScale))
   | ((v >= ZOOM_FLOOR) && (v <= sceneScale))) {
   //sceneScale = newValue;
   return true
   }
   false
   }
   */


  implicit def tuple2ToPVector(v:(Double,Double)) : PVector = {
    new PVector(v._1.toFloat,v._2.toFloat,0.0f)
  }
  implicit def tuple3ToPVector(v:(Double,Double,Double)) : PVector = {
    new PVector(v._1.toFloat,v._2.toFloat,v._3.toFloat)
  }
  
  implicit def pvectorToTuple2(v:PVector) : (Double,Double) = {
    (v.x.toDouble,v.y.toDouble)
  }

  implicit def pvectorToTuple3(v:PVector) : (Double,Double,Double) = {
    (v.x.toDouble,v.y.toDouble,v.z.toDouble)
  }

  protected def moveCameraAt(x:Double,y:Double,z:Double) {
    translate(x.toFloat, y.toFloat)
    scale(z.toFloat)
  }

  protected def setupCamera = moveCameraAt(_camera.position._1,
                                           _camera.position._2,
                                           _camera.zoom)
 
  protected def setBackground (c:(Int,Int,Int)) = {
    background(c._1, c._2, c._3)
  }

  protected def setFontSize(size:Int) = {
    textFont(_fonts.get(size))
  }

  protected def setColor (c:(Int,Int,Int)) = {
    fill(c._1,c._2,c._3)
  }

  protected def setLod (v:Int) = {
    bezierDetail(v)
  }
  protected def lineColor(c:(Int,Int,Int)) = {
    stroke(c._1,c._2,c._3)
  }
  protected def lineThickness(t:Double) = {
    strokeWeight(t.toFloat)
  }

  /*
   //println("Moving camera at: "+v)
   //var zr = v._3
   val t : PVector = _camera.position
   val c : PVector = _camera.center

   val newZ =
   var zr = 0
   if (zr < 1.0) { // ZOOM-IN
   zr /= 0.92
   if (zr >= 0.9) {
   zr = 1.0
   }
   if (v.tryToMultiplyZoom(Camera.zRatio)) {
   t.sub(Camera.center);
   t.mult(zr)
   t.add(Camera.center);
   } else {
   zr = 1.0f;
   // trigger an event: we hit the
   if (v.getLevel() == ViewLevel.MESO) {
   //System.out.println("GOING BACK TO MACRO VIEW");
   setView("macro");
   }
   }
   } else if (Czr > 1.0) { // ZOOM-OUT
   zr *= 0.92f;
   if (zr <= 1.1f) {
   zr = 1.0f;
   }
   if (v.tryToMultiplyZoom(Camera.zRatio)) {
   v.translation.sub(Camera.center);
   v.translation.mult(Camera.zRatio);
   v.translation.add(Camera.center);
   } else {
   zr = 1.0f;
   }
   }
   _camera.position = (v._1,v._2,zr)
   */

  val minZoom = 0.02
  val maxZoom = 180.0

  def limit(v:Double,min:Double,max:Double) : Double = {
    if (v < min) min else if (v > max) max else v
  }

  protected def zoomUpdated(v:Double) {}
  protected def positionUpdated(v:(Double,Double)) {}

  def zoom(zoomIn:Boolean) {

    _camera.lastMousePosition = (mouseX, mouseY)
    _camera.center = (mouseX, mouseY)

    val c : PVector = _camera.center
    val p : PVector = _camera.position

    // the basic roomRatio
    val zoomRatio = if (zoomIn) 1.3 else 0.7

    val oldZoom = _camera.zoom
    _camera.zoom = limit(_camera.zoom * zoomRatio, minZoom, maxZoom)
    zoomUpdated(_camera.zoom)

    p.sub(c)
    p.mult(zoomRatio.toFloat)
    p.add(c)
    _camera.position = p
    positionUpdated(_camera.position)
    println("camera position: "+_camera.position+" zoom: "+_camera.zoom)
  }

  /*
   def zoomAt(zoomIn:Boolean, position:(Double,Double), ratio:Double=1.2) {
   _camera.lastMousePosition = (mouseX, mouseY)
   _camera.center = position
   _camera.position = (_camera.position._1,
   _camera.position._2,
   if (zoomIn) (1.0 + ratio) else (1.0 - ratio))

   }*/

  def showSelectionCircle(radius:Double) {

    if (radius < 1) return
    
    scale((1.0/_camera.zoom).toFloat)
    translate(-_camera.position._1.toFloat, -_camera.position._2.toFloat)
    stroke(0, 0, 0, 40)
    strokeWeight(1.0f)
    fill(00, 100, 200, 29)
    drawDisk((mouseX, mouseY),radius)
    translate(_camera.position._1.toFloat, _camera.position._2.toFloat)
    scale(_camera.zoom.toFloat)
   
  }
    
  def stopAutoCentering {
    
  }
  
  override def mouseDragged {
    stopAutoCentering
    
     val p : PVector = _camera.position
     val t : PVector = _camera.position
     t.sub(_camera.lastMousePosition)
     _camera.lastMousePosition = (mouseX,mouseY)
     t.add(_camera.lastMousePosition)
     _camera.positionDelta = PVector.sub(p,t)
     _camera.dragged = true
     
  }

  override def mouseReleased {
   _camera.lastMousePosition = (mouseX, mouseY)
   _camera.dragged = false
  }

  override def mouseWheelMoved(e:MouseWheelEvent) {
    //println("got mouseWheelMoved")
    if (!(mouseX < 0
          | mouseX > width
          | mouseY < 0
          | mouseY > height)) {
      if (e.getUnitsToScroll != 0) {
        zoom(e.getWheelRotation < 0)
      }
    }
  }

}