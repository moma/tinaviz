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
import tinaviz.graph._
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
    size(1400, 900, PConstants.P2D)
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
          "file:///home/jbilcke/Checkouts/git/TINA/tinasoft.desktop/static/current.gexf"
          //"file:///Users/jbilcke/Checkouts/git/tina/grapheWhoswho/bipartite_graph.gexf"
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

  var nbVisibleNodes = 0
  var nbVisibleEdges = 0
  override def draw(): Unit = {

    // send some values
    Server ! "frameRate" -> frameRate.toInt

    // get some values in a non-blocking way (using futures)
    val scene = getIfPossible[Scene]("scene")
    val debug = getIfPossible[Boolean]("debug")
    val selectionRadius = getIfPossible[Double]("selectionRadius")


    _recenter(scene.graph, scene.graph.get[String]("camera.target"))

    // val centering = scene.graph.get[Boolean]("centering")

    // we need to manually move the camera
    // to the graph's center

    if (scene.graph.get[Boolean]("pause"))
      smooth
    else
      (if (nbVisibleEdges < 900) smooth else noSmooth)

    setBackground(scene.background)
    if (debug) {
      setColor(scene.foreground)
      setFontSize(9)
      //text("" + frameRate.toInt + " img/sec", 10f, 13f)
      text("drawing " + nbVisibleNodes + "/" + scene.nbNodes + " nodes (" + scene.graph.nbSingles + " singles), " + nbVisibleEdges + "/" + scene.nbEdges + " edges (" + frameRate.toInt + " img/sec)", 10f, 13f)
    }

    setupCamera // TODO use an immutable Camera (this is the reason for the selection disk bug)
    setLod(32)
    lineThickness(1)
    noFill

    val visibleNodes = scene.nodePositionLayer.zipWithIndex.filter {
      case (position, i) => isVisible(screenPosition(position))
    }
    nbVisibleNodes = visibleNodes.size

    def compareBySelection(i: Int, j: Int): Boolean = ( !scene.graph.selected(i) && scene.graph.selected(j) )

    //val visibleNodes = visibleNodesTmp.map { _._2 }.toList.sort(compareBySelection).toArray

    // TODO filter by weight, and show only the N biggers
    //  (Boolean, Int, (Double,Double),(Double,Double),Double, Color, Int)
    val edgeTmp = scene.edgePositionLayer.zipWithIndex map {
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
         scene.edgeWeightLayer(i),
         scene.edgeColorLayer(i),
         if (powd >= 10 && width >= 11) limit(PApplet.map(powd.toFloat, 10, width, 1, scene.maxLod), 1, scene.maxLod).toInt else 1)

        } else {
          (false,
           i,
           source,
           target,
           0.0,
          new Color (0.0,0.0,0.0),
           0)
        }
    }

    val edgeWeightIsPercentOfNodeSize = 0.3 // half of a node radius

    nbVisibleEdges = edgeTmp.filter{case (visible, i, source, target, weight, color, lod) => visible}.size
    edgeTmp foreach {
      case (visible, i, source, target, weight, color, lod) =>
        if (visible) {
          setLod(lod)
          lineColor(color)
          //Maths.map(weight, scene.)
          //println("weight: "+weight)

          // lineThickness(weight * getZoom)
            if (nbVisibleNodes < 30000) {
             val th = if (nbVisibleEdges < 2000) {
              //lineThickness(scene.graph.thickness(i))
              //lineThickness(Maths.map(scene.edgeWeightLayer(i),()) * getZoom)
               //Maths.map(weight,())
                //math.max(math.min(weight, 1.0),100.0)
               // take the two nodes and
               val (a,b) = scene.edgeIndexLayer(i)
               val m = math.min(scene.nodeSizeLayer(a),
                                scene.nodeSizeLayer(b))
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
        setColor(scene.nodeBorderColorLayer(i))
        scene.nodeShapeLayer(i) match {
          case 'Disk =>
            drawDisk(position, scene.nodeSizeLayer(i))
            setColor(scene.nodeColorLayer(i))
            drawDisk(position, scene.nodeSizeLayer(i) * 0.8)
          case x =>
            drawSquare(position, scene.nodeSizeLayer(i))
            setColor(scene.nodeColorLayer(i))
            drawSquare(position, scene.nodeSizeLayer(i) * 0.8)
        }
    }


    def compareBySize(i: Int, j: Int): Boolean = {
      val r1 = scene.nodeSizeLayer(i)
      val l1 = scene.nodeLabelLayer(i)
      val r2 = scene.nodeSizeLayer(j)
      val l2 = scene.nodeLabelLayer(j)
      if (r1 > r2) true else (if (r1 < r2) false else (l1.compareTo(l2) < 0))
    }
    val sortedLabelIDs = visibleNodes.map { _._2 }.toList.sort(compareBySize).toArray


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
        val weAreSelected = false// we don't care. else, use: scene.graph.selected(i)
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
            val whichIsSelected = false// we don't care. else, use: scene.graph.selected(j)
            val weTouchSomething = ((((np1._1 <= np2._1) && (np1._1 + w1 >= np2._1))
                                  || ((np1._1 >= np2._1) && (np1._1 <= np2._1 + w2)))
                                 && (((np1._2 <= np2._2) && (np1._2 + h1 >= np2._2))
                                  || ((np1._2 >= np2._2) && (np1._2 <= np2._2 + h2))))
            val whichIsLarger = if (r2 > r1) true else (if (r2 < r1) false else (scene.nodeLabelLayer(j).compareTo(scene.nodeLabelLayer(i)) > 0))
            //println("   weTouchSomething:"+weTouchSomething+" whichIsLarger: "+whichIsLarger+" L2: "+l2+" R2: "+r2+" h2: "+h2+" w2: "+w2+" x: "+np2._1+" y: "+np2._2)
            if (i == j) false else (weTouchSomething && (whichIsLarger || whichIsSelected))
        }
        setFontSize((r1 * getZoom).toInt)
        val col = if (weAreSelected) { scene.labelColor.alpha(1.0) } else { scene.labelColor.alpha(0.8) }
        setColor(col)
        // we can show the label if we are selected, or if we do not collide with a bigger one
        if ((!weHaveACollision) || weAreSelected) text(l1, np1._1, (np1._2 + (h1 / 2.0)).toInt)
    }

    showSelectionCircle(selectionRadius)
  }


  /**
   * Recenter
   */
  private def _recenter(g: Graph, mode : String) {

    mode match {
      case "all" =>
        //println("recentering to all")
      case "selection" =>
        //println("recentering to selection")

      // move it
      case "none" =>
        //println("recentering to none")
        return
      case err =>
        println("error")
        return
    }
    val w = width.toDouble
    val h = height.toDouble
    val cz = g.cameraZoom
    val cp = g.cameraPosition
    def model2screen(p: (Double,
      Double)): (Int,
      Int) = (((p._1 + cp._1) * cz).toInt,
      ((p._2 + cp._2) * cz).toInt)
    def screen2model(p: (Double,
      Double)): (Double,
      Double) = ((p._1 - cp._1) / cz,
      (p._2 - cp._2) / cz)

    // first we normalize the graph (although optional - this might interfer with the layout)
    //val h = normalizePositions(g)
    val (xMin, yMin, xMax, yMax) = (g.get[Double]("xMin"), g.get[Double]("yMin"),
      g.get[Double]("xMax"), g.get[Double]("yMax"))
    // now we want the coordinate within the screen
    val (a, b) = (model2screen(xMin, yMin), model2screen(xMax, yMax))
    val (sxMin, syMin, sxMax, syMax) = (a._1.toDouble, a._1.toDouble, b._2.toDouble, b._2.toDouble)
    //println("sxMin,syMin,sxMax,syMax = " + (sxMin, syMin, sxMax, syMax))

    // then we want to compute the difference
    val (xRatio, yRatio) = (abs(sxMax - sxMin) / width,
      abs(syMax - syMin) / height)

    //println("xRatio: " + xRatio + " yRatio: " + yRatio)
    val big = max(xRatio, yRatio)

    //println("big: " + big)
    // TODO should call the zoom updated callback as well
    //zoomWith(big)

    val pos =  if (mode.equals("selection") && g.selection.size > 0) g.selectionCenter else g.baryCenter

    //println("position: "+pos)
    var translate = new PVector()
    translate.add(new PVector(width / 2.0f, height / 2.0f, 0))
    val tmp = PVector.mult(new PVector(pos._1.toFloat, pos._2.toFloat), big.toFloat)
    translate.sub(tmp)
    updatePosition(translate)
  }

  /**
   * We override the zoomUpdated callback
   * this is called whenever the zoom is updated
   * value contains here the new value of the camera zoom
   */
  override def zoomUpdated(value: Double) {
    Server ! "camera.target" -> "none"
    Server ! "camera.zoom" -> value
  }

  /**
   * We override the positionUpdated callback
   *
   */
  override def positionUpdated(value: (Double, Double)) {
    Server ! "camera.target" -> "none"
    Server ! "camera.position" -> value
  }

  override def mouseUpdated(kind: Symbol,
                            side: Symbol,
                            count: Symbol,
                            position: (Double, Double)) {
    Server ! ("camera.mouse", kind, side, count, position)
  }

  /**
   * Called whenever a key is pressed by the user
   * We use here the processing-provided "key" variable, which give us the key code
   *
   */
  override def keyPressed() {
    key match {
      case 'p' => Server ! "pause" -> 'toggle
      case 'a' => Server ! "pause" -> 'toggle
      case 'n' => Server ! "drawing.nodes" -> 'toggle
      case 'l' => Server ! "drawing.edges" -> 'toggle
      case 'e' => Server ! ("export", "gexf")

      case 'c' => Server ! "filter.node.category" -> 'toggle
      case 'v' => Server ! "filter.view" -> 'toggle
      case 'r' => Server ! "camera.target" -> "all"
      case 's' => Server ! "camera.target" -> "selection"
      case 'u' => Server ! "select" -> ""

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
