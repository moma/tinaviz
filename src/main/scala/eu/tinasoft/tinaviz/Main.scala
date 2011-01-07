
package eu.tinasoft.tinaviz

import processing.core._

import eu.tinasoft.tinaviz.data.json.JsonParser
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

object Main {
  def main(args: Array[String]): Unit = {
    var frame = new javax.swing.JFrame("TinaViz")
    var applet = new Main()
    frame.getContentPane().add(applet)
    applet.init
    //init
    frame.pack
    frame.setVisible(true)
  }
}
class Main extends PApplet {
  
  val tinaviz : scala.actors.Actor = new Tinaviz()
  val fonts = new Fonts(this)

  override def setup(): Unit = {
    size(screenWidth - 200, screenHeight - 400, PConstants.P2D)
    frameRate(4)
    noSmooth
    textMode(PConstants.SCREEN)
    rectMode(PConstants.CENTER)
    bezierDetail(16)

    try {
      Browser.init(JSObject.getWindow(this), getParameter("js_context"))
    } catch {
      case exc:java.lang.NullPointerException =>
        println("Null pointer exception: "+exc)
        
      case exc:netscape.javascript.JSException =>
        println("Javascript exception: "+exc)

    }
    
    //val __brandingIcon = getParameter("branding_icon")
    //val __engine = getParameter("engine")
    
  }

  override def draw(): Unit = {
    
    tinaviz ! "profiler.fps" -> frameRate.toInt

    val scene : Scene = (tinaviz !? 'getScene) match { case s:Scene => s }
    setBackground(scene.background)

    (tinaviz !? "scene.debug") match {
      case true =>
        setColor(scene.foreground)
        text("" + frameRate.toInt + " img/sec", 10f, 13f)
    }

    val pause : Boolean = (tinaviz !? 'pause) match {
      case true => true
      case x => false
    }
    if (pause) return
    
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

  private def drawSquare(position:(Double,Double),radius:Double) = {
    rect(position._1.toFloat,position._2.toFloat,radius.toFloat,radius.toFloat)
  }
  private def drawDisk(position:(Double,Double),radius:Double) = {
    ellipse(position._1.toFloat,position._2.toFloat,radius.toFloat,radius.toFloat)
  }

  private def drawCurve(n1:(Double,Double), n2:(Double,Double)) = {
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
  private def setBackground (c:(Int,Int,Int)) = {
    background(c._1, c._2, c._3)
  }

  private def setFontSize(size:Int) = {
    textFont(fonts.get(size))
  }

  private def setColor (c:(Int,Int,Int)) = {
    fill(c._1,c._2,c._3)
  }

  private def setLod (v:Int) = {
    bezierDetail(v)
  }

  private def setThickness(t:Double) = {
    strokeWeight(t.toFloat)
  }




  // Called by Javascript

  def setPause(b:Boolean) = {
    true
  }
  
  def setView(s:String) = {
    s match {
      case "macro" =>
      case "meso" =>
    }
    true
  }

  def togglePause = {
    (tinaviz !? "pause" -> 'toggle) match {
      case b:Boolean => b
      case x => false
    }
  }
  /**
   * Deprecated
   */
  def toggleNodes = {
    true
  }
  /**
   * Deprecated
   */
  def toggleEdges = {
    true
  }
  /**
   * Deprecated
   */
  def toggleLabels = {
    true
  }
  /**
   * Deprecated
   */
  def toggleHD = {
    
  }

  def unselect = {

  }

  /**
   * Set a param
   * TODO: boolean sync?
   */
  def setParam(key:String,value:String,sync:Boolean) = {
    tinaviz ! key -> value
    println("ignoring sync: "+sync)
  }

  /**
   * 
   */
  def getNeighbourhood(view:String, rawJSONList:String) : Unit = {
    
  }
  
  /**
   * Update a Node in the current view, from it's UUID
   */
  def updateNode(str:String) = {
    // we help the JS developer by telling him if the JSON is valid or not
    val res = JsonParser.parse(str)
    
    if (!res.isDefined) throw new IllegalArgumentException("Error, invalid JSON!")
    tinaviz ! 'updateNode -> res.get
    
  }
}