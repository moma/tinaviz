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
    size(800, 600, PConstants.P2D)
    //size(screenWidth - 400, screenHeight - 400, PConstants.P2D)
    frameRate(30)
    //noSmooth
    smooth
    colorMode(PConstants.HSB, 1.0f)
    textMode(PConstants.SCREEN)
    rectMode(PConstants.CENTER)
    bezierDetail(18)

    // set some defaults
    setDefault("scene", new Scene())
    setDefault("debug", false)
    setDefault("pause", true)
    setDefault("selectionRadius", 10.0)


    addMouseWheelListener(this)

    Browser.start


    try {
      Browser.init(this, getParameter("js_context"))
      println("Connecting to web browser..")
    } catch {
      //case exc:NullPointerException =>
      // println("Null pointer exception: "+exc)
      //case exc:JSException =>
      //println("Javascript exception: "+exc)
      //tinaviz ! 'openURL -> "file:///home/jbilcke/Checkouts/git/TINA/tinasoft.desktop/static/tinaweb/default.gexf"
      case e: Exception =>
        println("Looking like we are not running in a web browser context..")
        tinaviz ! 'open -> new java.net.URL(
           //"file:///Users/jbilcke/Checkouts/git/tina/tinasoft.desktop/static/tinaweb/default.gexf"
         // "file:///Users/jbilcke/Checkouts/git/tina/grapheWhoswho/bipartite_graph.gexf"
          "file:///home/jbilcke/Checkouts/git/TINA/tinasoft.desktop/static/tinaweb/default.gexf"
          // "file:///home/jbilcke/Checkouts/git/TINA/tinaviz2/misc/bipartite_graph.gexf"
        )
    }
  }


  override def draw(): Unit = {

    // send some values
    tinaviz ! "frameRate" -> frameRate.toInt

    // get some values in a non-blocking way (using futures)
    val scene = getIfPossible[Scene]("scene")
    val debug = getIfPossible[Boolean]("debug")
    val selectionRadius = getIfPossible[Double]("selectionRadius")

    // drawing
    //smooth()

    setBackground(scene.background)
    if (debug) {
      setColor(scene.foreground)
      text("" + frameRate.toInt + " img/sec", 10f, 13f)
      text("drawing " + scene.nbNodes + " nodes, " + scene.nbEdges + " edges", 10f, 32f)
    }


    // TODO use an immutable Camera (this is the reason for the selection disk bug)
    setupCamera


    setLod(32)
    lineThickness(1)
    noFill

    scene.edgePositionLayer.zipWithIndex foreach {
      case ((source, target), i) =>
        val psource = screenPosition(source)
        val ptarget = screenPosition(target)

        val weight = scene.edgeWeightLayer(i)
        if (isVisible(psource) || isVisible(ptarget)) {
          val powd = distance(psource, ptarget)
          val lod = if (powd >= 10 && width >= 11) limit(PApplet.map(powd.toFloat, 10, width, 1, 120), 1, 120).toInt else 1
          setLod(lod)
          lineColor(scene.edgeColorLayer(i))
          //Maths.map(weight, scene.)
          //println("weight: "+weight)
         // lineThickness(weight * getScale)
          drawCurve(source, target)
        }
    }

    setLod(16)
    lineThickness(0)
    noStroke
    val visibleNodes = scene.nodePositionLayer.zipWithIndex.filter {
      case (position, i) => isVisible(screenPosition(position))
    }
    visibleNodes.foreach {
      case (position, i) =>
        val size = scene.nodeSizeLayer(i)
        val color = scene.nodeColorLayer(i)
        setColor(scene.nodeBorderColorLayer(i))
        scene.nodeShapeLayer(i) match {
          case 'Disk =>
            drawDisk(position, size)
            setColor(color)
            drawDisk(position, size * 0.8)
          case x =>
            drawSquare(position, size)
            setColor(color)
            drawSquare(position, size * 0.8)
        }
    }

    setColor(scene.labelColor)
    visibleNodes.foreach {
      case (position, i) =>
        val size = scene.nodeSizeLayer(i)
        val np = screenPosition(position._1 + size, position._2)

        setFontSize((size * getZoom).toInt)

        text(scene.nodeLabelLayer(i), np._1, np._2)

    }


    showSelectionCircle(selectionRadius)

  }

  override def zoomUpdated(value: Double) {
    tinaviz ! "camera.zoom" -> value
  }

  override def positionUpdated(value: (Double, Double)) {
    tinaviz ! "camera.position" -> value
  }

  override def mouseUpdated(kind:Symbol,
                            side:Symbol,
                            count:Symbol,
                            position: (Double, Double)) {
    tinaviz ! ("camera.mouse", kind, side, count, position)
  }


  override def keyPressed() {
    key match {
      case 'p' => tinaviz ! "pause" -> 'toggle
      case 'a' => tinaviz ! "pause" -> 'toggle
      case 'n' => tinaviz ! "drawing.nodes" -> 'toggle
      case 'e' => tinaviz ! "drawing.edges" -> 'toggle
      case 'c' => tinaviz ! "filter.node.category" -> 'toggle
      case 'v' => tinaviz ! "filter.view" -> 'toggle
      case 'r' => tinaviz ! "recenter"
      case PConstants.CODED =>
        val amount = 15
        keyCode match {
          case PConstants.UP => moveUp(amount)
          case PConstants.DOWN => moveDown(amount)
          case PConstants.LEFT => moveLeft(amount)
          case PConstants.RIGHT => moveRight(amount)
          case y =>

        }
      case x =>
        //
    }
  }
}
