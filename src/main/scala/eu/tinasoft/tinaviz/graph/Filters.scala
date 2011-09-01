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

import eu.tinasoft.tinaviz.util.Maths
import reflect.ValDef

object Filters {

  def category(g: Graph): Graph = {
    if (g.nbNodes == 0) return g

    var removeMe = Set.empty[Int]
    val category = g.currentCategory
    g.currentView match {
      case "macro" =>
        g.category.zipWithIndex map {
          case (cat, i) =>
            //print(" "+g.label(i)+" ("+g.category(i)+"): ")
            if (!category.equalsIgnoreCase(cat)) {
              //println("REMOVING")
              removeMe += i
            } else {
              //println("KEEPING")
            }
        }

      case "meso" =>
        g.selected.zipWithIndex foreach {
          case (f, i) => if (!f) {
            // remove the node which is not in our category
            if (!g.currentCategory.equalsIgnoreCase(g.category(i))) {
              //println("removing "+g.label(i)+" because: !"+g.currentCategory+".equalsIgnoreCase("+g.category(i)+")=="+(!g.currentCategory.equalsIgnoreCase(g.category(i))))
              removeMe += i
            } else {
              var keepThat = false
              // we remove nodes not connected to the selection
              g.selection.foreach {
                case j => if (g.hasAnyLink(i, j)) keepThat = true
              }
              if (!keepThat) {
                removeMe += i
              }
            }
          }
        }
    }

    // println("current graph has "+g.uuid.size+" nodes. curr category is "+g.currentCategory+". removed "+removeMe.size+" nodes ")
    //removeMe.foreach{
    //  case i => print(""+g.label(i)+" ("+g.category(i)+"), ")
    //}

    clean(g.remove(removeMe))
  }

  /**
   * Filter the Nodes weights
   */
  def nodeWeight2(g: Graph, ref: Graph): Graph = {
    if (g.nbNodes == 0) return g

    // update the graphical interface
    var (updatedA, updatedB) = (false, false)

    var removeMe = Set.empty[Int]
    g.weight.zipWithIndex.map {
      case (weight, i) =>
        (g.category(i) match {
          case "Document" => ref.nodeAWeightRange
          case "NGram" => ref.nodeBWeightRange
        }) match {
        case (minWeight, maxWeight) =>
          if (!(minWeight <= weight && weight <= maxWeight))
            if (!g.selected(i))
              removeMe += i
      }
    }
    g.remove(removeMe)
  }

  /**
   * Filter the Nodes weights
   */
  /*
  def nodeWeight(g: Graph, ref: Graph): Set[Int] = {
    if (g.nbNodes == 0) return Set.empty[Int]
    val rangeA = Maths.map(
      g.get[(Double, Double)]("filter.a.node.weight"),
      (0.0, 1.0),
      (ref.minANodeWeight, ref.maxANodeWeight))
    val rangeB = Maths.map(
      g.get[(Double, Double)]("filter.b.node.weight"),
      (0.0, 1.0),
      (ref.minBNodeWeight, ref.maxBNodeWeight))
    var removeMe = Set.empty[Int]
    g.weight.zipWithIndex.map {
      case (weight, i) =>
        val r = g.category(i) match {
          case "Document" => rangeA
          case "NGram" => rangeB
        }
        if (!(r._1 <= weight && weight <= r._2))
          if (!g.selected(i)) removeMe += i
    }
    removeMe
  }
  */

  /**
   * Filter the Nodes weights
   */
  def edgeWeight(g: Graph, ref: Graph): Graph = {
    if (g.nbNodes == 0) return g

    val newLinks = g.links.zipWithIndex map {
      case (links, i) =>
        links.filter {
          case (j, weight) =>
            if (!g.category(i).equalsIgnoreCase(g.category(j))) {
              true
            } else {
              (g.category(i) match {
                case "Document" => ref.edgeAWeightRange
                case "NGram" => ref.edgeBWeightRange
              }) match {
                case (minWeight, maxWeight) =>  (minWeight <= weight && weight <= maxWeight)
              }
            }
        }
    }
    clean(g + ("links" -> newLinks))
  }


  def weightToSize(g: Graph, ref: Graph): Graph = {
    if (g.nbNodes == 0) return g
    val sliderRange = (26.0, 60.0) //node size range

    // small adjustement ("hack") to fit sliders to data from TinasoftPytextminer
    val aratio = 0.6 * g.get[Double]("filter.a.node.size") // Document
    val bratio = 1.0 * g.get[Double]("filter.b.node.size") // NGram

    val aminmaxweight = (ref.minANodeWeight, ref.maxANodeWeight) // Document
    val bminmaxweight = (ref.minBNodeWeight, ref.maxBNodeWeight) // NGram
    //println("applyWeightToSize: " + ratio)
    val newSize = g.weight.zipWithIndex map {
      case (weight, i) =>

        if (g.category(i) equalsIgnoreCase "Document") {

          Maths.map(weight, aminmaxweight, sliderRange) * aratio

        } else {
          Maths.map(weight, bminmaxweight, sliderRange) * bratio

        }

    }
    g + ("size" -> newSize)
  }

  /**
   * Do some pre-processing, then send the final scene to the View
   * TODO check if this function is still important
   */
  def clean(g: Graph): Graph = {
    g + ("links" -> g.links.zipWithIndex.map {
      case (links, i) =>
        links.filter {
          case (j, weight) =>
            // in the case of mutual link, we have a bit of work to remove the link
            if (g.hasThisLink(j, i)) {
              // if i is bigger than j, we keep
              Functions.isBiggerThan(g, i, j)
              // in the case of non-mutual link (directed), there is nothing to do; we keep the link
            } else {
              true
            }
        }
    }.toArray)
  }
}
