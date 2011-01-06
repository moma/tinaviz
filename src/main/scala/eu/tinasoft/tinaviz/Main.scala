
package eu.tinasoft.tinaviz

import processing.core._

import netscape.javascript.JSObject

import org.daizoru._
import eu.tinasoft._

import tinaviz.drawing._

class Stats (
  val nbVisibleNodes:Int=0,
  val nbVisibleEdges:Int=0,
  val nbFilteredNodes:Int=0,
  val nbFilteredEdges:Int=0,
  val nbTotalNodes:Int=0,
  val nbTotalEdges:Int=0) {


}

object Main extends PApplet {
  
  val tinaviz : scala.actors.Actor = new Tinaviz()
  val fonts = new Fonts(this)
  val browser = new Browser(JSObject.getWindow(this), getParameter("js_context"))

  def main(args: Array[String]): Unit = {

    var frame = new javax.swing.JFrame("TinaViz")
    //var applet = Main
    frame.getContentPane().add(Main)
    //applet.init
    init
    frame.pack
    frame.setVisible(true)
  }
  
  override def setup(): Unit = {
    size(screenWidth - 200, screenHeight - 400, PConstants.P2D)
    frameRate(4)
    noSmooth
    textMode(PConstants.SCREEN)
    rectMode(PConstants.CENTER)
    bezierDetail(16)

    val __jsContext = getParameter("js_context")
    val __rootPrefix = getParameter("root_prefix")
    val __brandingIcon = getParameter("branding_icon")
    val __engine = getParameter("engine")
    
  }

  override def draw(): Unit = {
    
    tinaviz ! new Externals(frameRate.toInt)

    val scene : Scene = (tinaviz !? 'getScene) match { case s:Scene => s }
    
    setBackground(scene.background)

    if (scene.debug) {
      text("" + frameRate.toInt + " img/sec", 10f, 13f)
    }

    /*
     * EDGE DRAWING
     */
    scene.edges.foreach{ case e =>
        setLod(e.lod)
        setColor(e.color)
        setThickness(e.thickness)
        drawCurve(e.source, e.target)
    }

    /*
     * NODE DRAWING
     */
    bezierDetail(16)
    setThickness(1)
    scene.nodes.foreach{ case e =>
        setColor(e.color)
        e.shape match {
          case 'Disk => drawDisk(e.position, e.size)
          case x => drawSquare(e.position, e.size)
        }
    }

    /*
     * LABEL DRAWING
     */
    bezierDetail(16)
    setThickness(1)
    setColor(scene.labelColor)
    scene.labels.foreach{ case e =>
        setFontSize(e.size)
        text(e.text)
    }
  }

  def drawSquare(position:(Double,Double),radius:Double) = {
    rect(position._1.toFloat,position._2.toFloat,radius.toFloat,radius.toFloat)
  }
  def drawDisk(position:(Double,Double),radius:Double) = {
    ellipse(position._1.toFloat,position._2.toFloat,radius.toFloat,radius.toFloat)
  }

  def drawCurve(n1:(Double,Double), n2:(Double,Double)) = {
    drawCurve4(n1._1.toFloat,n1._2.toFloat,n2._1.toFloat,n2._2.toFloat)
  }

  def drawCurve4(n1x:Float, n1y:Float, n2x:Float, n2y:Float) = {

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
  def setBackground (c:(Int,Int,Int)) = {
    background(c._1, c._2, c._3)
  }

  def setFontSize(size:Int) = {
    textFont(fonts.get(size))
  }

  def setColor (c:(Int,Int,Int)) = {
    fill(c._1,c._2,c._3)
  }

  def setLod (v:Int) = {
    bezierDetail(v)
  }

  def setThickness(t:Double) = {
    strokeWeight(t.toFloat)
  }
}