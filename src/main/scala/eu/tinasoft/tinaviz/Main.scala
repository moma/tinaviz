
package eu.tinasoft.tinaviz

import processing.core._

import org.daizoru._
import eu.tinasoft._

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

class Main extends TApplet with Tinaviz {
  
  override def setup(): Unit = {
    size(screenWidth - 200, screenHeight - 400, PConstants.P2D)
    frameRate(4)
    noSmooth
    textMode(PConstants.SCREEN)
    rectMode(PConstants.CENTER)
    bezierDetail(16)

    try {
      Browser.init(netscape.javascript.JSObject.getWindow(this), getParameter("js_context"))
    } catch {
      case exc:java.lang.NullPointerException =>
        println("Null pointer exception: "+exc)
        
      case exc:netscape.javascript.JSException =>
        println("Javascript exception: "+exc)
        tinaviz ! 'openURL -> "file:///home/jbilcke/Checkouts/git/TINA/tinasoft.desktop/static/tinaweb/default.gexf"
   
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
        text("drawing " + scene.nodes.size + " nodes, "+scene.edges.size+" edges", 10f, 32f)
    }

    val pause : Boolean = (tinaviz !? "scene.pause") match {
      case true => true
      case x => false
    }
    if (pause) return
    
    /*
     * EDGE DRAWING
     */
    setLod(16)
    lineThickness(1)
    noFill
    scene.edges.foreach{ case e =>
        setLod(e.lod)
        lineColor(e.color)
        //lineThickness(e.thickness)
        drawCurve(e.source, e.target)
    }

    /*
     * NODE DRAWING
     */
    
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
     
    /*
     * LABEL DRAWING
     */
    setLod(16)
    lineThickness(1)
    setColor(scene.labelColor)
    scene.labels.foreach{ case e =>
        //setFontSize(e.size)
        text(e.text)
    }
  }
}