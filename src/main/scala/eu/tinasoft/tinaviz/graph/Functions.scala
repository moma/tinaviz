/************************************************************************
                                  Tinaviz
*************************************************************************
 This application is part of the Tinasoft project: http://tinasoft.eu
 Tinaviz main developer: julian.bilcke @ iscpif.fr  (twitter.com/flngr)

Copyright (C) 2009-2011 CREA Lab, CNRS/Ecole Polytechnique UMR 7656 (Fr)
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


}