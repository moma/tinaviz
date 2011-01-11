/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.util

object Vector {   
  implicit def fromDouble (p:(Double,Double)) = new Vector(p._1, p._2)
  implicit def toDouble (v:Vector) = (v.x,v.y)
  
  /**
   * Return the extremums for X (min,max) and Y (min,max)
   */
  def extremums(values:Seq[(Double,Double)]) : ((Double,Double),(Double,Double)) = {
    if (values.size == 0) return ((.0,.0),(.0,.0))
    var minX = Double.MaxValue
    var minY = Double.MaxValue
    var maxX = Double.MinValue
    var maxY = Double.MinValue
    values.foreach { 
      case n =>
        if (n._1 < minX) minX = n._1
        if (n._1 > maxX) maxX = n._1
        if (n._2 < minY) minX = n._2
        if (n._2 > maxY) maxX = n._2
    }
    ((minX,maxX),(minY,maxY))  
  }

}

case class Vector (val x:Double,val y:Double) {
  def += (p:(Double,Double)) = (x+p._1,y+p._2)
  def *= (p:(Double,Double)) = (x*p._1,y*p._2)
  def + (p:(Double,Double)) = (x+p._1,y+p._2)
  def * (p:(Double,Double)) = (x*p._1,y*p._2)

 def computeForce(f:Double,e:(Double,Double)) : (Double,Double) = {
    val dx = e._1 - x
    val dy = e._2 - y
    val d = math.sqrt(dx*dx + dy*dy)
    //println("  d: "+d)
    if (d!=0.0) ((dx / d) * f, (dy / d) * f) else (0.0,0.0)
  }
}
