
package eu.tinasoft.tinaviz

import processing.core._

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
  
  val viz : scala.actors.Actor = new MainController()
  val fonts = new Fonts(this)
   
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
    size(screenWidth - 200, screenHeight - 400, PConstants.P2D);
    frameRate(4)
    noSmooth
    textMode(PConstants.SCREEN)
    rectMode(PConstants.CENTER)
    bezierDetail(16)
    //noLoop

  }

  override def draw(): Unit = {
    
    val model : Model = (viz !? 'model) match { case m:Model => m }
    
    setBackground(model.background)

    if (model.debug) {
      text("" + frameRate.toInt + " img/sec", 10f, 13f)
    }

    model.edges.foreach{ case e =>

    }
    
    model.nodes.foreach{ case n =>
        
        n.shape match {
          case 'Disk => drawDisk(n.position, n.radius)
          case x => drawSquare(n.position, n.radius)
        }
    }

    setColor(model.labelColor)
    model.labels.foreach{ case l =>
        setFontSize(12)

        //float rad = n.getRadius()
        //text((n.isHighlighted()) ? n.label : n.shortLabel, screenX(nx + tmprad, ny + (tmprad / PI)), screenY(nx + tmprad, ny + (tmprad / PI)));
        //text((n.isHighlighted()) ? n.label : n.shortLabel, screenX(nx + rad, ny + (rad / PI)), screenY(nx + rad, ny + (rad / PI)));
        
    }
  }

  def drawSquare(position:(Double,Double),radius:Double) = {
    rect(position._1.toFloat,position._2.toFloat,radius.toFloat,radius.toFloat)
  }
  def drawDisk(position:(Double,Double),radius:Double) = {
    ellipse(position._1.toFloat,position._2.toFloat,radius.toFloat,radius.toFloat)
  }

  def drawCurve(n1x:Float, n1y:Float, n2x:Float, n2y:Float) = {

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
           n2x, n2y);
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
}