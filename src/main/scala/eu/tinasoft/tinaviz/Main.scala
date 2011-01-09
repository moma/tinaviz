
package eu.tinasoft.tinaviz

import javax.swing.JFrame

import netscape.javascript.JSException
import netscape.javascript.JSObject

import processing.core._

import org.daizoru._
import eu.tinasoft._

import Color._

object Main {
  def main(args: Array[String]): Unit = {
    var frame = new JFrame("TinaViz")
    var applet = new Main()
    frame.getContentPane().add(applet)
    applet.init
    frame.pack
    frame.setVisible(true)
  }
}

class Main extends TApplet with Tinaviz {
  
  override def setup(): Unit = {

    // set some defaults
    setDefault("scene", new Scene())
    setDefault("debug", false)
    setDefault("pause", true)
    setDefault("selectionRadius", 10.0)

    size(screenWidth - 100, screenHeight - 100, PConstants.P2D)
    frameRate(20)
    noSmooth
    textMode(PConstants.SCREEN)
    rectMode(PConstants.CENTER)
    bezierDetail(16)
    addMouseWheelListener(this)
    
    try {
      Browser.init(JSObject.getWindow(this), getParameter("js_context"))
    } catch {
      case exc:NullPointerException =>
        println("Null pointer exception: "+exc)
        
      case exc:JSException =>
        println("Javascript exception: "+exc)
        //tinaviz ! 'openURL -> "file:///home/jbilcke/Checkouts/git/TINA/tinasoft.desktop/static/tinaweb/default.gexf"
        tinaviz ! 'openURL -> "file:///Users/jbilcke/Checkouts/git/tina/tinasoft.desktop/static/tinaweb/default.gexf"
   
    }
    
    //val __brandingIcon = getParameter("branding_icon")
    //val __engine = getParameter("engine")
    
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
        //val powd = distance(e.source.screenPosition, e.target.screenPosition)
        //val modulator = if (width >= 10) limit(PApplet.map(powd.toFloat, 10, width, 1, 90), 1, 90) else 1
        //setLod(modulator.toInt)
        lineColor(scene.edgeColorLayer(i))
        //lineThickness(e.weight)
        drawCurve(source, target)
        i += 1
    }
/*
    setLod(16)
    lineThickness(0)
    noStroke
    scene.nodes.foreach{ case e =>
        setColor(e.color)
        e.shape match {
          case 'Disk => drawDisk(e.position, e.size)
          case x => drawSquare(e.position, e.size)
        }
    }

    setLod(16)
    lineThickness(1)
    setColor(scene.labelColor)
    scene.labels.foreach{ case e =>
        //setFontSize(e.size)
        text(e.text)
    }
*/
    showSelectionCircle(selectionRadius)

  }

  override def zoomUpdated(value:Double) {
    tinaviz ! "zoom" -> value
  }
  override def positionUpdated(value:(Double,Double)) {
    tinaviz ! "position" -> value
  }
}