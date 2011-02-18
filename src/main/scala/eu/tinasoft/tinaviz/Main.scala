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
    frameRate(30)
    smooth
    colorMode(PConstants.HSB, 1.0f)
    textMode(PConstants.SCREEN)
    rectMode(PConstants.CENTER)
    bezierDetail(18)

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
      case e: Exception =>
        println("Looking like we are not running in a web browser context..")
        tinaviz ! 'open -> new java.net.URL(
          //"file:///Users/jbilcke/Checkouts/git/tina/tinasoft.desktop/static/tinaweb/default.gexf"
          // "file:///Users/jbilcke/Checkouts/git/tina/grapheWhoswho/bipartite_graph.gexf"
          //"file:///home/david/fast/gitcode/tinaweb/FET67bipartite_graph_logjaccard_.gexf"
          "file:///home/jbilcke/Checkouts/git/TINA/tinaviz2/misc/bipartite_graph.gexf"
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

    setBackground(scene.background)
    if (debug) {
      setColor(scene.foreground)
      setFontSize(9)
      //text("" + frameRate.toInt + " img/sec", 10f, 13f)
      text("drawing " + scene.nbNodes + " nodes, " + scene.nbEdges + " edges (" + frameRate.toInt + " img/sec)", 10f, 13f)
    }

    setupCamera  // TODO use an immutable Camera (this is the reason for the selection disk bug)
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
          setLod(if (powd >= 10 && width >= 11) limit(PApplet.map(powd.toFloat, 10, width, 1, 120), 1, 120).toInt else 1)
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

    def compare(i:Int, j: Int) : Boolean = {
        val r1 = scene.nodeSizeLayer(i)
        val l1 = scene.nodeLabelLayer(i)
        val r2 = scene.nodeSizeLayer(j)
        val l2 = scene.nodeLabelLayer(j)
        val rez = if (r1 > r2) true else (if (r1 < r2) false else (scene.nodeLabelLayer(i).compareTo(scene.nodeLabelLayer(j)) < 0))
        //println("compare("+l1+","+l2+")="+rez)
        rez
    }

    val sortedLabelIDs = visibleNodes.map { _._2 }.toList.sort(compare).toArray

    setColor(scene.labelColor) // default color
    sortedLabelIDs.foreach {
      case (i) =>
        val p1 = scene.nodePositionLayer(i)
        val r1 = scene.nodeSizeLayer(i)
        val x1 = p1._1 + r1
        val y1 = p1._2
        val np1 = screenPosition(x1, y1)
        val l1 = scene.nodeLabelLayer(i)
        val h1 = setFontSize((r1 * getZoom).toInt)
         //println("r1: "+r1)
        val w1 = textWidth(l1) /// getZoom
        if (!sortedLabelIDs.exists {
          case (j) =>
            val p2 = scene.nodePositionLayer(j)
            val r2 = scene.nodeSizeLayer(j)
            val x2 = p2._1 + r2
            val y2 = p2._2
            val np2 = screenPosition(x2, y2)
            val l2 = scene.nodeLabelLayer(j)
            val h2 = setFontSize((r2 * getZoom).toInt)
            val w2 = textWidth(l2) /// getZoom //
            (((((x1 <= x2) && (x1 + w1 >= x2)) || ((x1 >= x2) && (x1 <= x2 + w2)))
              && (((y1 <= y2) && (y1 + h1 >= y2)) || ((y1 >= y2) && (y1 <= y2 + h2))))
              && (if (r1 > r2) true else (if (r1 < r2) false else (scene.nodeLabelLayer(j).compareTo(scene.nodeLabelLayer(i)) < 0))))
        }) text(l1, np1._1, np1._2)
    }

    showSelectionCircle(selectionRadius)

  }

  override def zoomUpdated(value: Double) {
    tinaviz ! "camera.zoom" -> value
  }

  override def positionUpdated(value: (Double, Double)) {
    tinaviz ! "camera.position" -> value
  }

  override def mouseUpdated(kind: Symbol,
                            side: Symbol,
                            count: Symbol,
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
