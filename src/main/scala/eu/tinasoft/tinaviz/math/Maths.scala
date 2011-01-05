/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.maths

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
}
