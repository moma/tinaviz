
package eu.tinasoft.tinaviz

import javax.swing.JFrame

import netscape.javascript.JSException

import processing.core._

import org.daizoru._
import eu.tinasoft._

import tinaviz.io.Browser
import tinaviz.scene._
import tinaviz.util._
import tinaviz.util.Color._
import math.Pi
/**
 * The Main object
 *
 * Only used when run from the command-line
 */
object Main {

  /**
   * main method
   */
  def main(args: Array[String]): Unit = {
    var frame = new JFrame("TinaViz")
    var applet = new Main()
    frame.getContentPane().add(applet)
    applet.init
    frame.pack
    frame.setVisible(true)
  }
}

/**
 * Main class
 *
 * This class inherits from TApplet, which itself inherits from PApplet,
 * a Processing-powered Applet. TApplet wraps some of the PApplet functions
 *
 * This class is extended with the Tinaviz trait, which does the real business
 * job: Tinaviz trait add a "tinaviz" actor, which act a bit like a master.
 * Tinaviz also add some useful functions to get parameters by key string, in
 * a cached and asynchronous way.
 *
 * @param
 * @return
 * @throws
 */
class Main extends TApplet with Client {
  
  override def setup(): Unit = {

    // set some defaults
    setDefault("scene", new Scene())
    setDefault("debug", false)
    setDefault("pause", true)
    setDefault("selectionRadius", 10.0)

    size(screenWidth - 100, screenHeight - 100, PConstants.P2D)
    frameRate(4)
    //noSmooth
    smooth
    colorMode(PConstants.HSB, 1.0f)
    textMode(PConstants.SCREEN)
    rectMode(PConstants.CENTER)
    bezierDetail(16)
    addMouseWheelListener(this)
    
    try {
      Browser.init(this, getParameter("js_context"))
    } catch {
      //case exc:NullPointerException =>
      // println("Null pointer exception: "+exc)
      //case exc:JSException =>
      //println("Javascript exception: "+exc)
      //tinaviz ! 'openURL -> "file:///home/jbilcke/Checkouts/git/TINA/tinasoft.desktop/static/tinaweb/default.gexf"
      case e:Exception =>
        tinaviz ! 'open -> new java.net.URL(
         // "file:///Users/jbilcke/Checkouts/git/tina/tinasoft.desktop/static/tinaweb/default.gexf"
        "file:///home/jbilcke/Checkouts/git/TINA/tinasoft.desktop/static/tinaweb/default.gexf"
        )
    }
  }


  override def draw(): Unit = {

    // send some values
    tinaviz ! "frameRate" -> frameRate.toInt

    // get some values in a non-blocking way (using futures)
    val scene = getIfPossible[Scene]("scene")
    val debug = getIfPossible[Boolean]("debug")
    val pause = getIfPossible[Boolean]("pause")
    val selectionRadius = getIfPossible[Double]("selectionRadius")
    
    // drawing
    //smooth()
    
    setBackground(scene.background)
    if (debug) {
      setColor(scene.foreground)
      text("" + frameRate.toInt + " img/sec", 10f, 13f)
      text("drawing " + scene.nbNodes + " nodes, "+scene.nbEdges+" edges", 10f, 32f)
    }
    if (pause) return
    

    // TODO use an immutable Camera (this is the reason for the selection disk bug)
    setupCamera


    setLod(32)
    lineThickness(1)
    noFill
    var i = 0
    scene.edgePositionLayer.foreach{
      case (source,target) =>
        val psource = screenPosition(source)
        val ptarget = screenPosition(target)
        if (isVisible(psource) || isVisible(ptarget)) {
          val powd = distance(psource, ptarget)
          val modulator = if (width >= 10) limit(PApplet.map(powd.toFloat, 10, width, 1, 90), 1, 90) else 1
          setLod(modulator.toInt)
          lineColor(scene.edgeColorLayer(i))
          //lineThickness(e.weight)
          drawCurve(source, target)
        }
        i += 1
    }

    setLod(16)
    lineThickness(0)
    noStroke

    i = 0
    scene.nodePositionLayer.foreach{
      case position =>
        val size = scene.nodeSizeLayer(i)
        setColor(scene.nodeBorderColorLayer(i))
        scene.nodeShapeLayer(i) match {
          case 'Disk => 
            drawDisk(position, size)
            setColor(scene.nodeColorLayer(i))
            drawDisk(position, size * 0.8)
          case x =>
            drawSquare(position, size)
            setColor(scene.nodeColorLayer(i))
            drawSquare(position, size * 0.8)
        }
        i += 1
    }


    setLod(16)
    lineThickness(1)
    setColor(scene.labelColor)
    i = 0
    scene.nodePositionLayer.foreach{
      case position =>
        val size = scene.nodeSizeLayer(i)
        setColor(scene.nodeColorLayer(i))
        val np = screenPosition(position._1 + size,
                                position._2 + size / Pi)
        text(scene.nodeLabelLayer(i), np._1,np._2)
        i += 1
    }


 
    showSelectionCircle(selectionRadius)

  }

  override def zoomUpdated(value:Double) {
    tinaviz ! "zoom" -> value
  }
  override def positionUpdated(value:(Double,Double)) {
    tinaviz ! "position" -> value
  }
}