/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.util

class Palette(val standard:Color,
              val dark:Color,
              val darker:Color,
              val light:Color,
              val lighter:Color) {

}
class Scheme(val primary:Palette,
             val secondary:Palette,
             val tertiary:Palette) {
  
}

object Samba extends Scheme (
  // blue
  new Palette(
    new Color(.5472, .65, .78),
    new Color(.5472, .66, .60),
    new Color(.5472, .96, .52),
    new Color(.5472, .65, .89),
    new Color(.5472, .48, .89)
  ),
  // yellow
  new Palette(
    new Color(.13055, .91, .99),
    new Color(.13055, .68, .76),
    new Color(.13055, .97, .66),
    new Color(.13055, .68, .99),
    new Color(.13055, .49, .99)
  ),
  // red
  new Palette(
    new Color(.9749, .91, .97),
    new Color(.9749, .69, .74),
    new Color(.9749, .97, .64),
    new Color(.9749, .67, .98),
    new Color(.9749, .50, .98)
  )
)

object LePort extends Scheme (
  // blue
  new Palette(
    new Color (0.54722, 0.86, 0.79),
    new Color (0.54722, 0.65, 0.60),
    new Color (0.54722, 0.92, 0.52),
    new Color (0.54722, 0.64, 0.89),
    new Color (0.54722, 0.47, 0.89)),

// yellow
  new Palette(
    new Color (0.13055, 0.90, 1.00),
    new Color (0.13055, 0.68, 0.76),
    new Color (0.13055, 0.95, 0.67),
    new Color (0.13055, 0.67, 1.00),
    new Color (0.13055, 0.49, 1.00)),
  // red
  new Palette(
    new Color (1.0, 0.89, 0.97),
    new Color (1.0, 0.68, 0.74),
    new Color (1.0, 0.95, 0.65),
    new Color (1.0, 0.67, 0.98),
    new Color (1.0, 0.49, 0.98))
)

object Rio extends Scheme (
  // green
  new Palette(
    new Color (0.247, 0.69, 0.95), // standard
    new Color (0.247, 0.69, 0.73), // dark
    new Color (0.247, 0.95, 0.67), // darker
    new Color (0.247, 0.67, 0.90), // light
    new Color (0.247, 0.26, 0.95)),// lighter

// yellow
  new Palette(
    new Color (0.13055, 0.90, 1.00),
    new Color (0.13055, 0.68, 0.76),
    new Color (0.13055, 0.95, 0.67),
    new Color (0.13055, 0.67, 1.00),
    new Color (0.13055, 0.49, 1.00)),
  // red
  new Palette(
    new Color (1.0, 0.89, 0.97),
    new Color (1.0, 0.68, 0.74),
    new Color (1.0, 0.95, 0.65),
    new Color (1.0, 0.67, 0.98),
    new Color (1.0, 0.49, 0.98))
)
object Color {
  
  // val samba = new Palette
  
  def fromRGBTuple3(c:(Double,Double,Double)) : Color  = {
    new Color(c._1,c._2,c._3)
  }
  def fromRGBTuple4(c:(Double,Double,Double,Double)) : Color  = {
    new Color(c._1,c._2,c._3,c._4)
  }
  def toRGBTuple3(c:Color) : (Double,Double,Double) = {
    val d = new java.awt.Color( java.awt.Color.HSBtoRGB(c.h.toFloat,c.s.toFloat,c.b.toFloat))
    (d.getRed,d.getGreen,d.getBlue)
  }
  def toRGBTuple4(c:Color) : (Double,Double,Double,Double) = {
    val d = new java.awt.Color( java.awt.Color.HSBtoRGB(c.h.toFloat,c.s.toFloat,c.b.toFloat))
    (d.getRed,d.getGreen,d.getBlue, 1.0)
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
  def hue(x:Double) : Color = {
    new Color(x,s,b,a)
  }
  def saturation(x:Double) : Color = {
    new Color(h,x,b,a)
  }
  def brightness(x:Double) : Color = {
    new Color(h,s,x,a)
  }
  def saturateBy(x:Double) : Color = {
    new Color(h,s*x,b,a)
  }
 def brightnessBy(x:Double) : Color = {
    new Color(h,s,b*x,a)
  }
  
  def alpha(a:Double) : Color = {
    new Color(h,s,b,a)
  }
  def alphaBy(f:Double) : Color = {
    new Color(h,s,b,a*f)
  }
  def toRGBTuple3 : (Double,Double,Double) = {
    val c = new java.awt.Color( java.awt.Color.HSBtoRGB(h.toFloat,s.toFloat,b.toFloat))
    (c.getRed,c.getGreen,c.getBlue)
  }
}
