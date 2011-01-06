/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

object Maths {
  
  def distance (p1: (Int, Int), p2: (Int, Int)) = {
    val (p1x, p1y) = p1
    val (p2x, p2y) = p2
    val dx = p1x - p2x
    val dy = p1y - p2y
    math.sqrt(dx*dx + dy*dy)
  }

  def isInCircle(p1 : (Int,Int), p2: (Int,Int), radius:Double)  = {
    distance (p1,p2) <= radius / 2.0
  }

  val PI_ON_TWO = math.Pi / 2.0
  val COS_PI_ON_TWO = math.cos(Maths.PI_ON_TWO)
  val SIN_PI_ON_TWO = math.sin(Maths.PI_ON_TWO)
  val COS_MINUS_PI_ON_TWO = math.cos(-Maths.PI_ON_TWO)
  val SIN_MINUS_PI_ON_TWO = math.sin(-Maths.PI_ON_TWO)
}
