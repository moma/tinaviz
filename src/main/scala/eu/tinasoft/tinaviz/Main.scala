
package eu.tinasoft.tinaviz

import javax.swing.JFrame

import netscape.javascript.JSException
import netscape.javascript.JSObject

import processing.core._

import org.daizoru._
import eu.tinasoft._

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
    size(screenWidth - 200, screenHeight - 400, PConstants.P2D)
    frameRate(4)
    noSmooth
    textMode(PConstants.SCREEN)
    rectMode(PConstants.CENTER)
    bezierDetail(16)

    try {
      Browser.init(JSObject.getWindow(this), getParameter("js_context"))
    } catch {
      case exc:NullPointerException =>
        println("Null pointer exception: "+exc)
        
      case exc:JSException =>
        println("Javascript exception: "+exc)
        tinaviz ! 'openURL -> "file:///home/jbilcke/Checkouts/git/TINA/tinasoft.desktop/static/tinaweb/default.gexf"
   
    }
    
    //val __brandingIcon = getParameter("branding_icon")
    //val __engine = getParameter("engine")
    
  }

  override def draw(): Unit = {
    
    tinaviz ! "frameRate" -> frameRate.toInt

    val scene : Scene = (tinaviz !? "scene") match { case s:Scene => s }
    setBackground(scene.background)

    (tinaviz !? "debug") match {
      case true =>
        setColor(scene.foreground)
        text("" + frameRate.toInt + " img/sec", 10f, 13f)
        text("drawing " + scene.nodes.size + " nodes, "+scene.edges.size+" edges", 10f, 32f)
    }

    val pause : Boolean = (tinaviz !? "pause") match {
      case true => true
      case x => false
    }
    if (pause) return
    
    moveCamera

    setLod(16)
    lineThickness(1)
    noFill
    scene.edges.foreach{ case e =>
        setLod(e.lod)
        lineColor(e.color)
        //lineThickness(e.thickness)
        drawCurve(e.source, e.target)
    }

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
    
    (tinaviz !? "selectionRadius") match {
      case d:Double => showSelectionCircle(d)
      case d:Float => showSelectionCircle(d.toDouble)
      case d:Int => showSelectionCircle(d.toDouble)
    } 
  }
}