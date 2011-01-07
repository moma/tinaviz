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
  
  def zoom(zoomIn:Boolean,ratio:Double=1.2) {
    _camera.lastMousePosition = (mouseX, mouseY)
    _camera.center = _camera.lastMousePosition
    _camera.zRatio = if (zoomIn) (1.0 + ratio) else (1.0 - ratio)
  }
  
  def zoomAt(zoomIn:Boolean, position:(Double,Double), ratio:Double=1.2) {
    _camera.lastMousePosition = (mouseX, mouseY)
    _camera.center = position
    _camera.zRatio = if (zoomIn) (1.0 + ratio) else (1.0 - ratio)
  }

    
  def stopAutoCentering {
    
  }
  
  override def mouseDragged {
    stopAutoCentering
    /*
     View v = Application.session.getView();
     PVector oldTranslation = new PVector(v.translation.x, v.translation.y, 0.0f);
     v.translation.sub(Camera.lastMousePosition);
     Camera.lastMousePosition.set(mouseX, mouseY, 0);
     v.translation.add(Camera.lastMousePosition);
     Camera.cameraDelta.set(PVector.sub(oldTranslation, v.translation));
     Camera.dragged = true;
     */
  }

  override def mouseReleased {
    //Camera.lastMousePosition.set(mouseX, mouseY, 0)
    //Camera.dragged = false
  }

  override def mouseWheelMoved(e:MouseWheelEvent) {
    if (!(mouseX < 0 | mouseX > width | mouseY < 0 | mouseY > height)) {
      if (e.getUnitsToScroll != 0) {
        zoom(e.getWheelRotation < 0)
      }
    }
  }
}