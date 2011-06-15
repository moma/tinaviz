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

package eu.tinasoft.tinaviz.graph

import math._

object Functions  {

  def normalizePositions(g:Graph) = {
    val h = g + ("position" -> (g.position.zipWithIndex map {
          case (position,i) =>
            (position._1 - g.baryCenter._1,
             position._2 - g.baryCenter._2)
        }))
    h + ("camera.position" -> (0.0,0.0))
  }

  def isBiggerThan(g:Graph,i:Int,j:Int) = {
    // the bigger win
    if (g.weight(i) > g.weight(j)) {
      true
    } else if (g.weight(i) < g.weight(j)) {
      false
    } else {
      // in the case of equal weights we fall back to label comparison, where the bigger win
      (g.label(i).compareTo(g.label(j)) > 0)
    }
  }


  /**
   * Are the given coordinate invisible?
   */
  def isVisible(p: (Int, Int)) = {
    val w = g.screenWidth / 48 // changed from 1/16 to 1/32
    val h = g.screenHeight / 128 // in height, we don't care too much
    ((p._1 > -w) && (p._1 < (g.screenWidth + w))
      && (p._2 > -h) && (p._2 < (g.screenHeight + h)))
  }

  /**
   * Are the given coordinate visible?
   */
  def isInvisible(p: (Int, Int)) = !isVisible(p)

  /**
   * TODO could be optimized, by using the reverse action (translate, zoom)
   * Thus we could use this function anywhere, if we have access to camera value
   */
  def screenPosition(p: (Double, Double)): (Int, Int) = screenPosition(p._1, p._2)


  /**
   * TODO could be optimized, by using the reverse action (translate, zoom)
   * Thus we could use this function anywhere, if we have access to camera value
   */
  def screenPosition(x: Double, y: Double): (Int, Int) = (screenX(x.toFloat, y.toFloat).toInt,
                                                          screenY(x.toFloat, y.toFloat).toInt)


  //def modelPosition(p:(Int,Int)) : (Double,Double) = modelPosition(p._1,p._2)
  //def modelPosition(x:Int,y:Int) : (Double,Double) = modelPosition(x,y)
  def modelPosition(p:(Double,Double)) : (Double,Double) = modelPosition(p._1,p._2)
  def modelPosition(x:Double,y:Double) : (Double,Double) = (
    (x.toDouble / _camera.zoom) - _camera.position._1,
    (y.toDouble / _camera.zoom) - _camera.position._2)

  /**
   * Get the size to the screen
   */
  def screenSize(s: Double): Int = {
    (s * _camera.zoom).toInt
  }
}