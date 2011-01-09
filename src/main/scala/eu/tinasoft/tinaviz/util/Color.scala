/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.util


object Color {
  
  def fromRGBTuple3(c:(Double,Double,Double)) : Color  = {
    new Color(c._1,c._2,c._2)
  }
  def fromRGBTuple4(c:(Double,Double,Double,Double)) : Color  = {
    new Color(c._1,c._2,c._2,c._4)
  }
  def toRGBTuple3(c:Color) : (Double,Double,Double) = {
    (c.h,c.s,c.b)
  }
  def toRGBTuple4(c:Color) : (Double,Double,Double,Double) = {
    (c.h,c.s,c.b,c.a)
  }
}
class Color(val h:Double=0.0,
            val s:Double=1.0,
            val b:Double=1.0,
            val a:Double=1.0) {

  def blend(c:Color) : Color = {
    new Color((h+c.h)/2,
              (s+c.s)/2,
              (b+c.b)/2,
              (a+c.a)/2)
  }

}
