package eu.tinasoft.tinaviz

import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
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
    var applet = new Main
    frame.getContentPane.add(applet)
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
    /*
     frame.setResizable(true)
     frame.addComponentListener(new ComponentAdapter() { 
     override def componentResized(e:ComponentEvent) { 
     if(e.getSource()==frame) { 
                     
     // HACK we need to update the screen ratio for recentering..
     // TODO put the following two calls in a "screen resized" callback"
     println("RESIZE CALLBACK")
     Server ! "screen.width" -> width
     Server ! "screen.height" -> height
     } 
     } 
     }
     )*/
    frameRate(30)
    smooth
    colorMode(PConstants.HSB, 1.0f)
    textMode(PConstants.SCREEN)
    rectMode(PConstants.CENTER)
    bezierDetail(18)
    setDefault("scene", new Scene)
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
        Server ! 'open -> new java.net.URL(
          //"file:///Users/jbilcke/Checkouts/git/tina/tinasoft.desktop/static/tinaweb/default.gexf"
          // "file:///Users/jbilcke/Checkouts/git/tina/grapheWhoswho/bipartite_graph.gexf"
          //"file:///home/david/fast/gitcode/tinaweb/FET67bipartite_graph_logjaccard_.gexf"
          //"file:///home/jbilcke/Checkouts/git/TINA/tinaviz2/misc/bipartite_graph.gexf"
          //"file:///home/jbilcke/Desktop/mini.gexf"
          //"file:///home/jbilcke/Documents/1_test_graph-graph.gexf"
          "file:///home/jbilcke/test-graph.gexf"
        )
    }
  }

  override def draw(): Unit = {

    // send some values
    Server ! "frameRate" -> frameRate.toInt

    // get some values in a non-blocking way (using futures)
    val scene = getIfPossible[Scene]("scene")
    val debug = getIfPossible[Boolean]("debug")
    val selectionRadius = getIfPossible[Double]("selectionRadius")
    
    // HACK we need to update the zoom. sometimes.
    // if (scene.graph.cameraZoom != getZoom) {
     //   zoomWith(scene.graph.cameraZoom)
    //} 
    

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
    
    val visibleNodes = scene.nodePositionLayer.zipWithIndex.filter {
      case (position, i) => isVisible(screenPosition(position))
    }
    // TODO filter by weight, and show only the N biggers
    scene.edgePositionLayer.zipWithIndex foreach {
      case ((source, target), i) =>
        val psource = screenPosition(source)
        val ptarget = screenPosition(target)

        val weight = scene.edgeWeightLayer(i)
        if (isVisible(psource) || isVisible(ptarget)) {
          val powd = distance(psource, ptarget)
          setLod(if (powd >= 10 && width >= 11) limit(PApplet.map(powd.toFloat, 10, width, 1, scene.maxLod), 1, scene.maxLod).toInt else 1)
          lineColor(scene.edgeColorLayer(i))
          //Maths.map(weight, scene.)
          //println("weight: "+weight)
          if (visibleNodes.size < 80) {
            //lineThickness(Maths.map(weight,()) * getScale)
          }
          // lineThickness(weight * getScale)
          if (visibleNodes.size < 100000) {
             drawCurve(source, target)
          }
        }
    }

    setLod(16)
    lineThickness(0)
    noStroke
   

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
        val w1 = textWidth(l1) /// getZoom
        // println("L1: "+l1+" r1: "+r1+" h1: "+h1+" w1: "+w1+" x: "+np1._1+" y: "+np1._2)
        val weHaveACollision = sortedLabelIDs.exists {
          case (j) =>
            val p2 = scene.nodePositionLayer(j)
            val r2 = scene.nodeSizeLayer(j)
            val x2 = p2._1 + r2
            val y2 = p2._2
            val np2 = screenPosition(x2, y2)
            val l2 = scene.nodeLabelLayer(j)
            val h2 = setFontSize((r2 * getZoom).toInt)
            val w2 = textWidth(l2) /// getZoom //
            val weTouchSomething = ((((np1._1 <= np2._1) && (np1._1 + w1 >= np2._1)) || ((np1._1 >= np2._1) && (np1._1 <= np2._1 + w2))) && (((np1._2 <= np2._2) && (np1._2 + h1 >= np2._2)) || ((np1._2 >= np2._2) && (np1._2 <= np2._2 + h2))))
            val whichIsLarger = if (r2 > r1) true else (if (r2 < r1) false else (scene.nodeLabelLayer(j).compareTo(scene.nodeLabelLayer(i)) > 0))
            //println("   weTouchSomething:"+weTouchSomething+" whichIsLarger: "+whichIsLarger+" L2: "+l2+" R2: "+r2+" h2: "+h2+" w2: "+w2+" x: "+np2._1+" y: "+np2._2)
            if (i==j) false else (weTouchSomething && whichIsLarger)
        }
        setFontSize((r1 * getZoom).toInt)
        //println("weHaveACollision? "+weHaveACollision)
        if (!weHaveACollision) text(l1, np1._1, np1._2)
    }

    showSelectionCircle(selectionRadius)

  }

  override def zoomUpdated(value: Double) {
    Server ! "camera.zoom" -> value
  }

  override def positionUpdated(value: (Double, Double)) {
    Server ! "camera.position" -> value
  }

  override def mouseUpdated(kind: Symbol,
                            side: Symbol,
                            count: Symbol,
                            position: (Double, Double)) {
    Server ! ("camera.mouse", kind, side, count, position)
  }


  override def keyPressed() {
    key match {
      case 'p' => Server ! "pause" -> 'toggle
      case 'a' => Server ! "pause" -> 'toggle
      case 'n' => Server ! "drawing.nodes" -> 'toggle
      case 'l' => Server ! "drawing.edges" -> 'toggle
      case 'e' => Server ! ("export","gexf")
        
      case 'c' => Server ! "filter.node.category" -> 'toggle
      case 'v' => Server ! "filter.view" -> 'toggle
      case 'r' =>
        // TODO should be in a "resized" callback
        Server ! "screen.width" -> width
        Server ! "screen.height" -> height
          
        Server ! "recenter"
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
