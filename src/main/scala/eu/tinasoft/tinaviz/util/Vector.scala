/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.util

object Vector {   
  implicit def fromDouble (p:(Double,Double)) = new Vector(p._1, p._2)
  implicit def toDouble (v:Vector) = (v.x,v.y)
}

class Vector (val x:Double,val y:Double) {
  def += (p:(Double,Double)) = (x+p._1,y+p._2)
  def *= (p:(Double,Double)) = (x*p._1,y*p._2)
}