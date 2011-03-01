package eu.tinasoft.tinaviz.graph

/**
 * Created by IntelliJ IDEA.
 * User: jbilcke
 * Date: 2/17/11
 * Time: 12:33 PM
 * To change this template use File | Settings | File Templates.
 */

import math._

object Functions  {
  /**
   * Return a new graph which is centered
   * we can center it using XY normalization, or camera, or camera force.. for the moment we keep it simple
   */
  
  def recenter (g:Graph) = {
    val width = g.get[Int]("screen.width").toDouble
    val height = g.get[Int]("screen.height").toDouble
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
    val (xMin,yMin,
         xMax,yMax) = (g.get[Double]("xMin"),g.get[Double]("yMin"),
                       g.get[Double]("xMax"),g.get[Double]("yMax"))
    // now we want the coordinate within the screen
    val (a,b) = (model2screen(xMin,yMin), model2screen(xMax,yMax))
    val (sxMin,syMin,
         sxMax,syMax) = (a._1.toDouble,a._1.toDouble,
                         b._2.toDouble,b._2.toDouble)
    println("sxMin,syMin,sxMax,syMax = "+(sxMin,syMin,sxMax,syMax))
    
    // then we want to compute the difference
    val (xRatio,yRatio) = (abs(sxMax-sxMin) / width,
                           abs(syMax-syMin) / height)
    
    println("xRatio: "+xRatio+" yRatio: "+yRatio)
    val big = max(xRatio,yRatio)
    
    println("big: "+big)
    
    // we compute the difference in size
    // then we
    
   // val h = g + (
    //  "camera.zoom" -> (  g.cameraZoom * big  )
   // )
    
   // h
    //h + ("camera.position" -> (0.0,0.0))
    g
  } 
  def normalizePositions(g:Graph) = {
    val h = g + ("position" -> (g.position.zipWithIndex map {
          case (position,i) =>
            (position._1 - g.baryCenter._1,
             position._2 - g.baryCenter._2)
        }))
    h + ("camera.position" -> (0.0,0.0))
  }

  def isBiggerThan(g:Graph,i:Int,j:Int) = {
    // the bigger win
    if (g.weight(i) > g.weight(j)) {
      true
    } else if (g.weight(i) < g.weight(j)) {
      false
    } else {
      // in the case of equal weights we fall back to label comparison, where the bigger win
      (g.label(i).compareTo(g.label(j)) > 0)
    }
  }
}