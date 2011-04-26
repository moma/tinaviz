package eu.tinasoft.tinaviz

import java.util.concurrent.atomic.AtomicReference
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import javax.swing.JFrame

import netscape.javascript.JSException

import processing.core._
import processing.pdf.PGraphicsPDF

import eu.tinasoft._

import tinaviz.io.Browser
import tinaviz.scene._
import tinaviz.pipeline._
import tinaviz.util._
import tinaviz.util.Color._
import tinaviz.graph._
import tinaviz.layout.Layout
import math._




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
    /*
    frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    frame.addWindowListener(new WindowAdapter {
      public void windowClosing(e:WindowEvent) {
        Server -> 'quit
        Browser -> 'quit
        System.exit(0)
      }
    })
    */

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
    size(1200, 800, PConstants.P2D)
    frameRate(35)
    colorMode(PConstants.HSB, 1.0f)
    textMode(PConstants.SCREEN)
    rectMode(PConstants.CENTER)
    bezierDetail(18)
    setDefault("output", new Graph)
    setDefault("debug", false)
    setDefault("pause", true)
    setDefault("selectionRadius", 10.0)

    addMouseWheelListener(this)

    /* In the JDK's appletviewer, selecting the Restart menu item calls stop() and then start().
     * Selecting the Reload menu item calls stop(), destroy(), and init(), in that order.
     *  (Normally the byte codes will also be reloaded and the HTML file reread though Netscape has a problem with this.)
     */
    Browser.start
    Workflow.start
    Server.start
    Layout.start


    try {
      Browser.init(this, getParameter("js_context"))
      println("Connecting to web browser..")
    } catch {
      case e: Exception =>
        println("Looking like we are not running in a web browser context..")
        Server ! 'open -> new java.net.URL(
          // "file:///home/jbilcke/Checkouts/git/TINA/tinasoft.desktop/sessions/badgraph/gexf/PseudoInclusion_logJaccard_FET-graph.gexf"
          // "file:///home/jbilcke/Checkouts/git/TINA/tinasoft.desktop/static/tinaweb/default.gexf"
          //"file:///Users/jbilcke/Checkouts/git/tina/grapheWhoswho/bipartite_graph.gexf"

         // "file:///home/jbilcke/Checkouts/git/TINA/tinasoft.desktop/sessions/package_test/gexf/Cooccurrences_sharedNGrams_FET-graph.gexf"
         "file:///home/jbilcke/Dropbox/Shared/Tina/test.gexf"
          //"file:///home/david/fast/gitcode/tinaweb/FET67bipartite_graph_logjaccard_.gexf"
          //"file:///home/jbilcke/Checkouts/git/TINA/tinaviz2/misc/bipartite_graph.gexf"

          // standard one
          //"file:///home/jbilcke/Checkouts/git/TINA/tinaviz2/misc/phylo.gexf"

          // seems buggy
          // "file:///home/jbilcke/Desktop/from_Batch_10_to_FET-graph.gexf"

          // "file:///home/jbilcke/Desktop/largescalegraph.gexf"
          // "file:///home/jbilcke/Checkouts/git/TINA/tinasoft.desktop/sessions/fetXX/gexf/FET-graph.gexf"

          //"file:///home/jbilcke/Documents/1_test_graph-graph.gexf"
          //"file:///home/jbilcke/test-graph.gexf"
        )
    }
  }

  var doZoom: Symbol = 'none
  var export: String = "none"

  var nbVisibleNodes = 0
  var nbVisibleEdges = 0

  override def draw(): Unit = {

    // send some values
    Server ! "frameRate" -> frameRate.toInt
    Server ! "camera.zoom" -> getZoom

    val g = Pipeline.output
    //println("Main: Pipeline.output.nbNodes: "+Pipeline.output.nbNodes)
    val debug = getIfPossible[Boolean]("debug")
    val selectionRadius = getIfPossible[Double]("selectionRadius")
    if (g.pause) smooth else if (nbVisibleEdges < 600) smooth else noSmooth

    export match {
      case "PDF" =>
        smooth
        beginRecord(PConstants.PDF, "graph.pdf")

      case "PNG" =>
        smooth


      case "GEXF" => Server ! ("export", "GEXF")

      case any =>
    }


    doZoom match {
      case 'in =>
        zoom(true)
        doZoom = 'none
      case 'out =>
        zoom(false)
        doZoom = 'none
      case any =>
    }

    _recenter(g, g.get[String]("camera.target"))

    export match {
      case "PDF" =>
        // no background for PDF files (translucent)
      case "PNG" =>
        // no background for PNG files (translucent)
      case any =>
        setBackground(g.currentView match {
          case "macro" => new Color(0.0, 0.0, 1.0)
          case "meso" => new Color(0.1416, 0.1, 1.0) // jaunâtre
          case any => new Color(0.0, 0.0, 1.0)
        })
    }


    if (debug) {
      setColor(new Color(0.0, 0.0, 0.0))
      setFontSize(9, false)
      //text("" + frameRate.toInt + " img/sec", 10f, 13f)
      text("drawing " + nbVisibleNodes + "/" + g.nbNodes + " nodes (" + g.nbSingles + " singles), " + nbVisibleEdges + "/" + g.nbEdges + " edges (" + frameRate.toInt + " img/sec) zoom: "+getZoom, 10f, 13f)

    }
    //updateCameraEngine
    setupCamera // TODO use an immutable Camera (this is the reason for the selection disk bug)
    setLod(32)
    lineThickness(1)
    noFill

    val visibleNodes = g.position.zipWithIndex.filter {
      case (position, i) => isVisible(screenPosition(position))
    }
    nbVisibleNodes = visibleNodes.size

    def compareBySelection(i: Int, j: Int): Boolean = (!g.selected(i) && g.selected(j))

    //val visibleNodes = visibleNodesTmp.map { _._2 }.toList.sort(compareBySelection).toArray

    // TODO filter by weight, and show only the N biggers
    //  (Boolean, Int, (Double,Double),(Double,Double),Double, Color, Int)
    val edgeTmp = g.renderEdgePosition.zipWithIndex map {
      case ((source, target), i) =>
        val psource = screenPosition(source)
        val ptarget = screenPosition(target)
        val visible = (isVisible(psource) || isVisible(ptarget))
        if (visible) {
          val powd = distance(psource, ptarget)
          (true,
            i,
            source,
            target,
            g.renderEdgeWeight(i),
            g.renderEdgeColor(i),
            if (powd >= 10 && width >= 11) limit(PApplet.map(powd.toFloat, 10, width, 1, 120), 1, 120).toInt else 1)

        } else {
          (false,
            i,
            source,
            target,
            0.0,
            new Color(0.0, 0.0, 0.0),
            0)
        }
    }

    val edgeWeightIsPercentOfNodeSize = 0.35 // 1/3 of a node radius for good looking edges

    nbVisibleEdges = edgeTmp.filter {
      case (visible, i, source, target, weight, color, lod) => visible
    }.size
    edgeTmp foreach {
      case (visible, i, source, target, weight, color, lod) =>
        if (visible && !g.selected(g.renderEdgeIndex(i)._1)) {
          setLod(lod)
          lineColor(color)
          if (nbVisibleNodes < 30000) {
            val th = if (nbVisibleEdges < 2000) {
              val (a, b) = g.renderEdgeIndex(i)
              val m = math.min(g.size(a),
                g.size(b))
              val wz = m * getZoom * edgeWeightIsPercentOfNodeSize
              if (wz < 1.0) 1.0 else (if (wz > 5.0) 5.0 else wz)
            } else {
              1.0
            }

            lineThickness(th)
            drawCurve(source, target)
          }
        }
    }

    nbVisibleEdges = edgeTmp.filter {
      case (visible, i, source, target, weight, color, lod) => visible
    }.size
    edgeTmp foreach {
      case (visible, i, source, target, weight, color, lod) =>
        if (visible && g.selected(g.renderEdgeIndex(i)._1)) {
          setLod(lod)
          lineColor(color)
          if (nbVisibleNodes < 30000) {
            val th = if (nbVisibleEdges < 2000) {
              val (a, b) = g.renderEdgeIndex(i)
              val m = math.min(g.size(a),
                g.size(b))
              val wz = m * getZoom * edgeWeightIsPercentOfNodeSize
              if (wz < 1.0) 1.0 else (if (wz > 5.0) 5.0 else wz)
            } else {
              1.0
            }

            lineThickness(th)
            drawCurve(source, target)
          }
        }
    }


    /**
     * Print the shapes (with color and size)
     */
    setLod(16)
    lineThickness(0)
    noStroke
    visibleNodes.foreach {
      case (position, i) =>
        setColor(g.renderNodeBorderColor(i))
        g.renderNodeShape(i) match {
          case 'Disk =>
            drawDisk(position, g.size(i))
            setColor(g.renderNodeColor(i))
            drawDisk(position, g.size(i) * 0.75)
          case x =>
            drawSquare(position, g.size(i))
            setColor(g.renderNodeColor(i))
            drawSquare(position, g.size(i) * 0.75)
        }
    }

    if (debug) {
      setColor(new Color(0.2, 1.0, 1.0, 0.7))
      drawDisk((0.0, 0.0), 10.0 / getZoom)    // JAUNE c'est en 0.0

      //setColor(new Color(0.2, 1.0, 1.0, 0.8))
      //drawDisk(g.baryCenter, 10.0 / getZoom)

      setColor(new Color(0.5, 1.0, 1.0, 0.7))    // bleu c'est le centre not single
      drawDisk(g.notSinglesCenter, 12.0 / getZoom)

      //
      // setColor(new Color(0.5, 1.0, 1.0, 0.8))
      //drawDisk(g.notSinglesCenter, 12.0 / getZoom)

      setColor(new Color(0.8, 1.0, 1.0, 0.7))
      drawDisk(g.selectionCenter, 14.0 / getZoom)  // violet, la selection center
    }

    def compareBySize(i: Int, j: Int): Boolean = {
      val r1 = g.size(i)
      val l1 = g.label(i)
      val r2 = g.size(j)
      val l2 = g.label(j)
      if (r1 > r2) true else (if (r1 < r2) false else (l1.compareTo(l2) < 0))
    }
    val sortedLabelIDs = visibleNodes.map {
      _._2
    }.toList.sort(compareBySize).toArray

    sortedLabelIDs.foreach {
      case (i) =>
        val p1 = g.position(i)
        val r1 = g.size(i)
        val x1 = p1._1 + r1
        val y1 = p1._2
        val np1 = screenPosition(x1, y1)
        val l1 = g.label(i)
        val b1 =  (g.selected(i) || g.highlighted(i)) // F1 the text should be bold if node selected or highlighted
        val h1 = setFontSize((r1 * getZoom).toInt, b1)
        val w1 = textWidth(l1) /// getZoom
        // println("L1: "+l1+" r1: "+r1+" h1: "+h1+" w1: "+w1+" x: "+np1._1+" y: "+np1._2)
        //val weAreSelected = false // we don't care. else, use: g.selected(i)
        val weAreSelected = g.selected(i)
        val weHaveACollision = sortedLabelIDs.exists {
          case (j) =>
            val p2 = g.position(j)
            val r2 = g.size(j)
            val x2 = p2._1 + r2
            val y2 = p2._2
            val np2 = screenPosition(x2, y2)
            val l2 = g.label(j)
            val b2 = (g.selected(j) || g.highlighted(j)) // SIDE EFFECT of F1
            val h2 = setFontSize((r2 * getZoom).toInt, b2)
            val w2 = textWidth(l2) /// getZoom //
            //val whichIsSelected = false // we don't care. else, use: scene.graph.selected(j)
            val whichIsSelected = g.selected(j)
            val weTouchSomething = ((((np1._1 <= np2._1) && (np1._1 + w1 >= np2._1))
              || ((np1._1 >= np2._1) && (np1._1 <= np2._1 + w2)))
              && (((np1._2 <= np2._2) && (np1._2 + h1 >= np2._2))
              || ((np1._2 >= np2._2) && (np1._2 <= np2._2 + h2))))
            val whichIsLarger = if (r2 > r1) true else (if (r2 < r1) false else (g.label(j).compareTo(g.label(i)) > 0))
            //println("   weTouchSomething:"+weTouchSomething+" whichIsLarger: "+whichIsLarger+" L2: "+l2+" R2: "+r2+" h2: "+h2+" w2: "+w2+" x: "+np2._1+" y: "+np2._2)
            if (i == j) false else (weTouchSomething && (whichIsLarger || whichIsSelected))
        }
        setFontSize((r1 * getZoom).toInt, b1)
        val col = if (weAreSelected) {
          new Color(0.0, 1.0, 0.0).alpha(1.0)
        } else {
          new Color(0.0, 1.0, 0.0).alpha(0.8)
        }
        setColor(col)
        // we can show the label if we are selected, or if we do not collide with a bigger one
        if ((!weHaveACollision) || weAreSelected) text(l1, np1._1, (np1._2 + (h1 / 2.0)).toInt)
    }


    export match {
      case "PDF" => endRecord()
      case "PNG" => save("graph.png")
      case any =>
    }
    export = "none"

    showSelectionCircle(selectionRadius)

  }

  /**
   * Recenter
   */
  private def _recenter(g: Graph, mode: String) {
    mode match {
      case "all" =>
      case "selection" =>
      case "none" =>
        return
      case err =>
        //println("error")
        return
    }
    //println("recentering will be done!")
    //val (w,h) = (width.toDouble - 60.0, height.toDouble - 60.0)
    //val (w,h) = (width.toDouble - 60.0, height.toDouble - 60.0)
    val (w,h) = (width.toDouble * 0.70, height.toDouble * 0.70)
    val (cz,cp) = (getZoom,getPosition)

    //def model2screen(p: (Double, Double)): (Int, Int) = (((p._1 + cp._1) * cz).toInt, ((p._2 + cp._2) * cz).toInt)
    //def screen2model(p: (Double,Double)): (Double, Double) = ((p._1 - cp._1) / cz, (p._2 - cp._2) / cz)
     val centerOnSelection = mode.equalsIgnoreCase("selection") && g.currentView.equalsIgnoreCase("macro") && g.selectionNeighbourhood.size > 1
      // spaghetti code
    val ratio = (
      if (centerOnSelection) {
         (abs(g.xMinSelectionNeighbourhood - g.xMaxSelectionNeighbourhood) * getZoom,
          abs(g.yMinSelectionNeighbourhood - g.yMaxSelectionNeighbourhood) * getZoom) // TODO we could use g.selection(0)
      } else  {
         (abs(g.xMin - g.xMax) * getZoom, abs(g.yMin - g.yMax) * getZoom)  // size to screen
      }
    ) match { case (gwidth,gheight) => max(gwidth / w, gheight / h) }
    val pos = if (centerOnSelection) g.selectionNeighbourhoodCenter else g.notSinglesCenter

    //if (abs(ratio) > 1.0001 || abs(ratio) < 0.9999) {


      var translate = new PVector(width.toFloat / 2.0f, height.toFloat / 2.0f, 0)
      translate.sub(PVector.mult(new PVector(pos._1.toFloat, pos._2.toFloat), getZoom.toFloat))
      translate.set(translate.x, translate.y, 0) // FIXME ugly hack, seems a bugs from the browser..
      updatePositionSilent(translate)
      //println("selectionNeighbourhood.size: "+g.selectionNeighbourhood.size+" pos: "+pos+"  ratio: "+ratio)
      if (g.selectionNeighbourhood.size != 1) {
      if (ratio != 0.0) updateZoomSilent(getZoom / (
             // if (ratio < 1.0) (ratio * 2.0) else (ratio / 2.0)
             ratio
        ))
       } else {
         updateZoomSilent(3.0) // hack: we update the zoom but we do not trigger an event (the "zoom changed" event is reserved for actions induced by users)
      }
    //}
  }

  /**
   * We override the zoomUpdated callback
   * this is called whenever the zoom is updated
   * value contains here the new value of the camera zoom
   */
  override def zoomUpdated(value: Double) {


    // TODO use the nap
    //Server ! "camera.target" -> "none"


    Server ! "camera.zoom" -> value
  }

  /**
   * We override the positionUpdated callback
   *
   */
  override def positionUpdated(value: (Double, Double)) {


    // TODO use the nap
    //Server ! "camera.target" -> "none"

    Server ! "camera.position" -> value
  }

  override def mouseUpdated(kind: Symbol,
                            side: Symbol,
                            count: Symbol,
                            position: (Double, Double)) {
    //println("mouseUpdated: camera.mouse, kind: "+kind+", side: "+side+", count: "+count+", position: "+position+"")
    Server ! ("camera.mouse", kind, side, count, position)
  }

  /**
   * Called whenever a key is pressed by the user
   * We use here the processing-provided "key" variable, which give us the key code
   *
   */
  override def keyPressed() {
    key match {
    //case 'p' => Server ! "pause" -> 'toggle
      case 'a' => Server ! "pause" -> 'toggle

      case 'n' => Server ! "drawing.nodes" -> 'toggle
      case 'l' => Server ! "drawing.edges" -> 'toggle

      case 'x' => export = "GEXF"
      case 'f' => export = "PDF"
      case 'g' => export = "PNG"

      case 'c' => Server ! "filter.node.category" -> 'toggle
      case 'v' => Server ! "filter.view" -> 'toggle

      case 'r' => Server ! "camera.target" -> "all"
      case 's' => Server ! "camera.target" -> "selection"
      case 'h' => Server ! "camera.target" -> "none"
      case 'u' => Server ! "select" -> ""

      case 'd' =>
        Server ! "debug" -> 'toggle

      case 'p' =>
        doZoom = 'in
      case 'm' =>
        doZoom = 'out

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

  override def start() {
    super.start()
    //println("started..")
    //Server ! "pause" -> 'toggle
  }

  override def stop() {
    super.stop()
    //println("stopped..")
    //Server ! "pause" -> false
  }

  override def destroy() {
    println("Main.scala: sending exit signal to Server")
    Server ! 'exit
    println("Main.scala: calling super.destroy()")
    super.destroy()
  }
}
