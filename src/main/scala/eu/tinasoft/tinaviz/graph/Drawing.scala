package eu.tinasoft.tinaviz.graph

/**
 * Created by IntelliJ IDEA.
 * User: jbilcke
 * Date: 6/8/11
 * Time: 12:08 PM
 * To change this template use File | Settings | File Templates.
 */

import math._
import eu.tinasoft.tinaviz.util.Maths
import eu.tinasoft.tinaviz.util.Color

object Drawing {

  def nodeColor(g: Graph) = {
    g.selected.zipWithIndex map {
      case (s, i) =>
        val mode = if (s) 'selected else if (g.highlighted(i)) 'highlighted else if (g.selectionValid) 'unselected else 'default
        if (g.originalColor(i).undef) {
            val color = (g.category(i) match {
              case "Document" => g.colorScheme.primary
              case "NGram" => g.colorScheme.tertiary
              case other => g.colorScheme.secondary
            })
            mode match {
              case 'selected => color.darker.saturateBy(1.00)
              case 'highlighted => color.darker.saturateBy(0.85)
              case 'unselected => color.standard.saturateBy(0.80)
              case default => color.standard.saturateBy(0.90)
            }
        }  else {
                 mode match {
              case 'selected => g.originalColor(i).brightnessBy(0.90).saturateBy(1.00)
              case 'highlighted => g.originalColor(i).brightnessBy(0.90).saturateBy(0.85)
              case 'unselected => g.originalColor(i).saturateBy(0.80)
              case default => g.originalColor(i).saturateBy(0.90)
            }
        }
    }
  }

  /**
   * compute the node border
   */
  def nodeBorderColor(g: Graph) = {

    val darkerColor = new Color(0.0, 1.0, 0.0).alpha(0.8)

    g.selected.zipWithIndex map {

      case (s, i) =>
        val mode = if (s) 'selected else if (g.highlighted(i)) 'highlighted else if (g.selectionValid) 'unselected else 'default
        if (g.originalColor(i).undef) {
            val color = (g.category(i) match {
              case "Document" => g.colorScheme.primary
              case "NGram" => g.colorScheme.tertiary
              case other => g.colorScheme.secondary
            })
            mode match {
              case 'selected => darkerColor
              case 'highlighted => darkerColor.saturateBy(0.90)
              case 'unselected => color.darker.saturation(0.90)
              case default => color.darker.saturation(1.0)
            }
        } else {
          mode match {
              case 'selected => darkerColor
              case 'highlighted => darkerColor.saturateBy(0.90)
              case 'unselected => g.originalColor(i).brightnessBy(0.80).saturation(0.90)
              case default => g.originalColor(i).brightnessBy(0.80).saturation(1.0)
            }
        }
    }
  }


  /**
   * compute the edge color to screen
   */
  def edgeColor(g: Graph) = {
    val darkerColor = new Color(0.0, 0.0, 0.23)
    var tmpColor = List.empty[Color]
    val aextremums = (g.minAEdgeWeight, g.maxAEdgeWeight)
    val bextremums = (g.minBEdgeWeight, g.maxBEdgeWeight)
    def colorate(i:Int, category: String) = (
      if (g.originalColor(i).undef) {
      category match {
      case "Document" => g.colorScheme.primary.standard
      case "NGram" => g.colorScheme.tertiary.standard
      case other => g.colorScheme.secondary.standard
    }
      } else {
         g.originalColor(i)
      }
    )

    def getExtremum(category: String) = (category match {
      case "Document" => aextremums
      case "NGram" => bextremums
      case any => aextremums
    })

    val target = (0.4, 1.0)
    g.links.zipWithIndex map {
      case (mapIntDouble, from) =>
        val modeFrom = if (g.selected(from)) 'selected else if (g.highlighted(from)) 'highlighted else if (g.selectionValid) 'unselected else 'default
        val catFrom = g.category(from)
        val extr = getExtremum(catFrom)
        mapIntDouble foreach {
          case (to, weight) =>
            val catTo = g.category(to)
            val modeTo = if (g.selected(to)) 'selected else if (g.highlighted(to)) 'highlighted else if (g.selectionValid) 'unselected else 'default

            tmpColor ::= ((modeFrom, modeTo) match {
              
              case ('selected, any) => darkerColor.alpha(Maths.map(weight, extr, (0.86, 0.98))) // previously: (0.86, 0.98)
              case ('highlighted, any) => darkerColor.alpha(Maths.map(weight, extr, (0.60, 0.95)))
              
              // color mutual links
              case (any, 'selected) => darkerColor.alpha(Maths.map(weight, extr, (0.86, 0.98)))
              case (any, 'highlighted) => darkerColor.alpha(Maths.map(weight, extr, (0.60, 0.95)))
              
              // or else..
              case (any1, any2) =>
                if (g.selectionValid) {
                  // unselected
                  val t = colorate(from,catFrom)
                  t.blend(colorate(to,catTo)).saturateBy(0.60).alpha(Maths.map(weight, extr, (0.75, 0.94)))
                } else {
                  val t = colorate(from,catFrom)
                  t.blend(colorate(to,catTo)).saturateBy(0.68).alpha(Maths.map(weight, extr, (0.60, 0.90)))
                }
            })

        }
    }
    tmpColor.toArray
  }


  /**
   * compute the edge position to screen
   */
  /*
  def edgePosition (g:Graph) = {
    var t = List.empty[((Double, Double), (Double, Double))]
    g.links.zipWithIndex foreach {
      case (links, i) =>
        links.zipWithIndex foreach {
          case ((j, weight), _j) => t ::= (g.position(i), g.position(j))
        }
    }
    t.toArray
  }*/


  /**
   * compute the edge size to screen
   */
  def edgeSize(g: Graph) = {
    (for ((links, i) <- g.links.zipWithIndex; (j, weight) <- links) yield {
      val sizes = (g.size(i), g.size(j))
      val avgSize = (sizes._1 + sizes._2) / 2.0
      val w = Maths.limit(avgSize, Maths.min(sizes), Maths.max(sizes))
      // print("  w: "+w)
      //val r = weight * :1.0
      //val r = 1.0 * 1.0
      //println("  r: "+r)
      //val
      w
    }).toArray
  }

  def nodeShape(g: Graph) = g.category.map {
    case "Document" => 'Square
    case "NGram" => 'Disk
    case any => 'Square
  }

  def labelColor(g: Graph): Array[Color] = ((g.selected).zip(g.highlighted).map {
    case (selected, highlighted) => new Color(0.0, 1.0, 0.0).alpha(if (selected || highlighted) 1.0 else 0.8)
  }).toArray

  def renderedLabel(g: Graph): Array[String] = ((g.selected).zip(g.highlighted).zip(g.label).zip(g.shortLabel).map {
    case (((selected, highlighted), label), shortLabel) => if (selected || highlighted) shortLabel else label
  }).toArray
}