
package eu.tinasoft.tinaviz

import org.daizoru._
import eu.tinasoft._

import tinaviz.drawing._

import actors._
import Actor._
import processing.core._

object Main extends PApplet {
  
  val viz : Actor = new MainController()
      
  def main(args: Array[String]): Unit = {

    var frame = new javax.swing.JFrame("TinaViz")
    var applet = Main
    frame.getContentPane().add(applet)
    applet.init
    frame.pack
    frame.setVisible(true)
  }
  
  override def setup(): Unit = {
    size(400, 300, PConstants.P2D)
    //noLoop
  }

  override def draw(): Unit = {
    
    // blocking call to get the model
    val model : Model = (viz ?! 'model) match {
      case m:Model => m
    }
    
    background(model.background)
    
    var i=0
    while(i < 100) {
      pushMatrix
      fill(random(255))
      translate(random(width), random(height))
      //box(random(10, 70))
      popMatrix
      i += 1
    }
  }
  
  def background (c:(Int,Int,Int)) : Void = {
        background(c._1, c._2, c._3)
  }

}