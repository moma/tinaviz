/************************************************************************
                                  Tinaviz
*************************************************************************
 This application is part of the Tinasoft project: http://tinasoft.eu
 Tinaviz main developer: julian.bilcke @ iscpif.fr  (twitter.com/flngr)

 Copyright (C) 2009-2011 CREA Lab, CNRS/Ecole Polytechnique UMR 7656 (Fr)

 This program is free software: you can redistribute it and/or modify it
 under the terms of the GNU General Public License as published by the
 Free Software Foundation, either version 3 of the License, or (at your
 option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program. If not, see <http://www.gnu.org/licenses/>.
************************************************************************/

package eu.tinasoft.tinaviz.util

//import org.processing.PApplet
import util.Random

object Maths {
  
  val rnd = new Random(4565)

  def distance (p:(Double,Double), p2: (Double, Double)) = {
    val dx = p._1 - p2._1
    val dy = p._2 - p2._2
    math.sqrt(dx*dx + dy*dy)
  }

 //def isInRange(p:(Double,Double), p2: (Double,Double), radius:Double) = p.distance(p2) <= (radius / 2.0)

  val PI = math.Pi
  val PI_ON_TWO = math.Pi / 2.0
  val COS_PI_ON_TWO = math.cos(Maths.PI_ON_TWO)
  val SIN_PI_ON_TWO = math.sin(Maths.PI_ON_TWO)
  val COS_MINUS_PI_ON_TWO = math.cos(-Maths.PI_ON_TWO)
  val SIN_MINUS_PI_ON_TWO = math.sin(-Maths.PI_ON_TWO)

  def nextInt = rnd.nextInt
  
  def random(max:Double) = max * rnd.nextDouble
  def random(min:Double,max:Double) = ((max-min) * rnd.nextDouble) - min
  def random() = rnd.nextDouble
  
  def randomBool = (rnd.nextDouble < 0.5)

  def map (value:(Double,Double), valueRange:(Double,Double), targetRange:(Double,Double)) : (Double,Double) = {
    (map(value._1, valueRange, targetRange), map(value._2, valueRange, targetRange))
  }
  /**
   * imitation of Processing's map function, with different downsides, I hope
   */
  def map (value:Double, valueRange:(Double,Double), tr:(Double,Double)) : Double = {
    val vr = if (valueRange._1 > valueRange._2) (valueRange._2,valueRange._1) else valueRange
    val v = if (value < vr._1) vr._1 else { if (value > vr._2) vr._2 else value }
    val r = if (vr._2-vr._1 == 0) 1.0 else ((v-vr._1) / (vr._2-vr._1))
    if (tr._1 > tr._2) 
      (tr._2 - ((tr._2 - tr._1) * r))
    else 
       (tr._1 + ((tr._2 - tr._1) * r))
  }



  def limit(v: Double, min: Double, max: Double): Double = {
    if (v < min) min else if (v > max) max else v
  }


  def min(a:Double,b:Double) = {
     if (a < b) a else b
  }
  def max(a:Double,b:Double) = {
     if (a > b) a else b
  }
  def min(ab:(Double,Double)) = {
     if (ab._1 < ab._2) ab._1 else ab._2
  }
  def max(ab:(Double,Double)) = {
     if (ab._1 > ab._2) ab._1 else ab._2
  }
}
