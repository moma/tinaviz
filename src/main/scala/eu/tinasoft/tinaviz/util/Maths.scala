/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.util

import util.Random

object Maths {
  
  val rnd = new Random(4565)

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
  def map (value:Double, valueRange:(Double,Double), targetRange:(Double,Double)) : Double = {
    val vr = if (valueRange._1 > valueRange._2) (valueRange._2,valueRange._1) else valueRange
    val tr = if (targetRange._1 > targetRange._2) (targetRange._2,targetRange._1) else targetRange
    val v = if (value < vr._1) vr._1 else { if (value > vr._2) vr._2 else value }
    val r = if (vr._2-vr._1 == 0) 1.0 else ((v-vr._1) / (vr._2-vr._1))
    (tr._1 + ((tr._2 - tr._1) * r))
  }
}
