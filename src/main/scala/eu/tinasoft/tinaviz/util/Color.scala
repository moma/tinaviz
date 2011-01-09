/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.util

class ColorSet(val standard:Color,
               val dark:Color,
               val darker:Color,
               val light:Color,
               val lighter:Color) {

}
class Palette(val primary:ColorSet,
              val secondary:ColorSet,
              val tertiary:ColorSet) {
  
}

object Samba extends Palette (
  // blue
  new ColorSet(
    new Color(.5472, .65, .78),
    new Color(.5472, .66, .60),
    new Color(.5472, .96, .52),
    new Color(.5472, .65, .89),
    new Color(.5472, .48, .89)
  ),
  // yellow
  new ColorSet(
    new Color(.13055, .91, .99),
    new Color(.13055, .68, .76),
    new Color(.13055, .97, .66),
    new Color(.13055, .68, .99),
    new Color(.13055, .49, .99)
  ),
  // red
  new ColorSet(
    new Color(.9749, .91, .97),
    new Color(.9749, .69, .74),
    new Color(.9749, .97, .64),
    new Color(.9749, .67, .98),
    new Color(.9749, .50, .98)
  )
)
object Color {


  // val samba = new Palette
  
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
class Color(val h:Double=1.0,
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
