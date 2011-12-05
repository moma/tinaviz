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
   * compute the edge weights
   */
  def edgeWeight (g:Graph) = {
    var t = List.empty[Double]
    g.links.zipWithIndex foreach {
      case (links, i) =>
        links.zipWithIndex foreach {
          case ((j, weight), _j) => t ::= weight
        }
    }
    t.toArray
  }
  /**
   * compute the edge position to screen
   */
  def edgeIndex (g:Graph) = {
    var t = List.empty[(Int, Int)]
    g.links.zipWithIndex foreach {
      case (links, i) =>
        links.zipWithIndex foreach {
          case ((j, weight), _j) => t ::= (i, j)
        }
    }
    t.toArray
  }

  /**
   * Generic function to make a label nicer
   * Richard "Aphex Twin" James - University of Weird Music
   * will give:
   * Richard "Aphex Tw..
   */
  def labelCurator(text: String, len: Int, separator: String) = {
    (
      if ((separator.size > 0) && text.contains(separator)) text.split(separator)(0)
      else text
      ) match {
      case text =>
        if (text.size > len) ("" + (text.grouped(len).toList)(0) + "..")
        else text
    }
  }

  /**
   * Tina-specific function to make labels nicer
   */
  def myLabelCurator(label: String, selected: Boolean = false) = if (selected) labelCurator(label, 55, " - ") else labelCurator(label, 19, " - ")


  /**
   * Are the given coordinate invisible?
   */

  def isVisible(g:Graph,p: (Int, Int)) = {
    val w = g.window._1 / 48 // changed from 1/16 to 1/32
    val h = g.window._2 / 128 // in height, we don't care too much
    ((p._1 > -w) && (p._1 < (g.window._1 + w))
      && (p._2 > -h) && (p._2 < (g.window._2 + h)))
  }


  /**
   * Are the given coordinate visible?
   */

  def isInvisible(g:Graph,p: (Int, Int)) = !isVisible(g,p)

  /**
   * TODO could be optimized, by using the reverse action (translate, zoom)
   * Thus we could use this function anywhere, if we have access to camera value
   */

  /*
  def screenPosition(g:Graph,p: (Double, Double)): (Int, Int) = screenPosition(g,p._1, p._2)

   */

  /**
   * TODO could be optimized, by using the reverse action (translate, zoom)
   * Thus we could use this function anywhere, if we have access to camera value
   */

   /*
  def screenPosition(g:Graph,x: Double, y: Double): (Int, Int) = (screenX(x.toFloat, y.toFloat).toInt,
                                                          screenY(x.toFloat, y.toFloat).toInt)
  */

   // position in the model
  /*def modelPosition(g:Graph,p:(Int,Int)) : (Double,Double) = modelPosition(g,p._1,p._2)
  def modelPosition(g:Graph,x:Int,y:Int) : (Double,Double) = modelPosition(g,x,y)
  */
  /*
  def modelPosition(g:Graph, p:(Double,Double)) : (Double,Double) = modelPosition(g,p._1,p._2)
  def modelPosition(g:Graph, x:Double,y:Double) : (Double,Double) = (
    (x.toDouble / g.cameraZoom) - g.cameraPosition._1,
    (y.toDouble / g.cameraZoom) - g.cameraPosition._2)
    */
  /**
   * Get the size to the screen
   */

  def screenSize(g: Graph, s: Double): Int = {
    (s * g.cameraZoom).toInt
  }

}