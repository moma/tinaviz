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

import processing.core.PVector
import math._

object Vector {   
  implicit def fromDouble (p:(Double,Double)) = new Vector(p._1, p._2)
  implicit def toDouble (v:Vector) = (v.x,v.y)
  implicit def toPVector (v:Vector) = new PVector(v.x.toFloat,v.y.toFloat)
  implicit def fromPVector (p:PVector) = new Vector(p.x.toDouble,p.y.toDouble)
  //implicit def doubleToPVector(p:(Double,Double)) = new PVector(p._1.toFloat,p._2.toFloat)
  //implicit def pVectorToDouble(p:PVector) = (p.x.toDouble,p.y.toDouble)

  
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
  //def += (p:(Double,Double)) = (x+p._1,y+p._2)
  //def *= (p:(Double,Double)) = (x*p._1,y*p._2)
  def + (p:(Double,Double)) = (x+p._1,y+p._2)
  def - (p:(Double,Double)) = (x-p._1,y-p._2)
  def * (p:(Double,Double)) = (x*p._1,y*p._2)

  def dist (p: (Double, Double)) = {
    val dx = x - p._1
    val dy = y - p._2
    sqrt(dx*dx + dy*dy)
  }


  

  def sqroot(a:Double,b:Double) = {
    def e(d:Double) = if (d < 1.0) (10 * pow(10,abs(log10(d).toInt))).toInt else  1
    val c = max(e(a),e(b))
    sqrt(a*a*c*c + b*b*c*c) / c
  }
  
  def isInRange(p: (Double,Double), radius:Double) = dist(p) <= (radius / 2.0)

  def attract(f:Double,e:(Double,Double)) : (Double,Double) = {
    val dx = e._1 - x
    val dy = e._2 - y
    val td = sqrt(dx*dx+dy*dy) * 0.5 //* 0.8
    val d = if (td < 0.01) 0.01 else td
    //println("  d: "+d)
    (d * f, d * f)
  }
  def repulseLess(f:Double,e:(Double,Double)) : (Double,Double) = {
    val dx = e._1 - x
    val dy = e._2 - y
    val td = sqrt(dx*dx+dy*dy) * 0.5 //* 0.8
    val d = if (td < 0.0001) 0.0001 else td
    //println("  d: "+d)
    (d * f, d * f)
  }
    def attractLess(f:Double,e:(Double,Double)) : (Double,Double) = {
    val dx = e._1 - x
    val dy = e._2 - y
    val td = sqrt(dx*dx+dy*dy) * 0.5 //* 0.8
    val d = if (td < 0.0001) 0.0001 else td
    //println("  d: "+d)
    //val ddx = if (dx > 0) (d * f) else (- d * f)
    //val ddy = if (dy > 0) (d * f) else (- d * f)
     (dx / (d * f), dy / (d * f))
  }
  
   def repulse(f:Double,e:(Double,Double)) : (Double,Double) = {
    val dx = e._1 - x
    val dy = e._2 - y
    val td = sqrt(dx*dx+dy*dy) * 0.5 //* 0.8
    val d = if (td < 0.01) 0.01 else td
    //println("  d: "+d)
    (dx / (d * f), dy / (d * f))
  }

  // stronger when closer
  def computeForceDeflector(f:Double,e:(Double,Double)) : (Double,Double) = {
    val dx = e._1 - x
    val dy = e._2 - y
    var d = sqrt(dx*dx+dy*dy) + 0.000001 //* 0.8
    val f2 = 1./ d 
   ((dx / d) * (f * f2), (dy / d) * (f * f2)) 
  }
  
  // stronger when closer
  def computeForceLimiter(f:Double,e:(Double,Double)) : (Double,Double) = {
    val dx = e._1 - x
    val dy = e._2 - y
    var d = sqrt(dx*dx+dy*dy) //* 0.8
    val f2 = if (d!=0.0) 1./d else 0.
    if (d!=0.0) ((dx / d) * (f * f2), (dy / d) * (f * f2)) else (dx * f * f2,dy * f * f2)
  }

  def absLimit(t:(Double,Double),l:(Double,Double)) : (Double,Double) = {
    val ax = math.abs(x)
    val ay = math.abs(y)
     (if (ax > t._1) (if (ax < t._2) x else { if (x > 0) (l._2) else (-l._2) }) else { l._1 },
         if (ay > t._1) (if (ay < t._2) y else { if (y > 0) (l._2) else (-l._2) }) else { l._1 })
  }
  
  /**************************************************/
    def _computeForce(f:Double,e:(Double,Double)) : (Double,Double) = {
    val dx = e._1 - x
    val dy = e._2 - y
    val d = sqrt(dx*dx+dy*dy) //* 0.8
    //println("  d: "+d)
    if (d > 0.1) ((dx / d) * f, (dy / d) * f) else (dx * f,dy * f)
  }
  
   def _computeLessForce(f:Double,e:(Double,Double)) : (Double,Double) = {
    val dx = e._1 - x
    val dy = e._2 - y
    val td = (sqrt(dx*dx+dy*dy))  //* 0.8
    val d = sqrt(td)
    //println("  d: "+d)
    if (abs(d) > 0.001) ((dx / d) * f, (dy / d) * f) else (0.0,0.0)
  }
   def _computeForceToRadius(f:Double,r:Double,e:(Double,Double)) : (Double,Double) = {
    val dx = e._1 - r - x
    val dy = e._2 - r - y
    val td = (sqrt(dx*dx+dy*dy))  //* 0.8
    val d = sqrt(td)
    //println("  d: "+d)
    if (abs(d) > 0.001) ((dx / d) * f, (dy / d) * f) else (0.0,0.0)
  }
  
  
  // stronger when closer
  def _computeForceDeflector(f:Double,e:(Double,Double)) : (Double,Double) = {
    val dx = e._1 - x
    val dy = e._2 - y
    var d = sqrt(dx*dx+dy*dy) + 0.000001 //* 0.8
    val f2 = 1./ d 
   ((dx / d) * (f * f2), (dy / d) * (f * f2)) 
  }
  
  // stronger when closer
  def _computeForceLimiter(f:Double,e:(Double,Double)) : (Double,Double) = {
    val dx = e._1 - x
    val dy = e._2 - y
    var d = sqrt(dx*dx+dy*dy) //* 0.8
    val f2 = if (d!=0.0) 1./d else 0.
    if (d!=0.0) ((dx / d) * (f * f2), (dy / d) * (f * f2)) else (dx * f * f2,dy * f * f2)
  }
}
