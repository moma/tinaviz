/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

object Color {
  implicit def fromTuple3(c:(Int,Int,Int)) : Color  = {
    new Color(c._1,c._2,c._2)
  }
  implicit def fromTuple4(c:(Int,Int,Int,Int)) : Color  = {
    new Color(c._1,c._2,c._2,c._4)
  }
  implicit def toTuple3(c:Color) : (Int,Int,Int) = {
    (c.r,c.g,c.b)
  }
  implicit def toTuple4(c:Color) : (Int,Int,Int,Int) = {
    (c.r,c.g,c.b,c.a)
  }
}
class Color(val r:Int=0,val g:Int=0,val b:Int=0, val a:Int=0) {


}
