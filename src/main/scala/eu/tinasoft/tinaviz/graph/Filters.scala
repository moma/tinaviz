/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import eu.tinasoft.tinaviz.util.Maths

object Filters {
  
  /**
   * Filter the Nodes weights
   */
  def nodeWeight(g: Graph): Graph = {
    if (g.nbNodes == 0) return g
    val range = Maths.map(
      g.get[(Double, Double)]("filter.node.weight"),
      (0.0, 1.0),
      (g.get[Double]("minNodeWeight"), g.get[Double]("maxNodeWeight")))
    var removeMe = Set.empty[Int]
    g.weight.zipWithIndex.map { 
      case (weight, i) => 
        if (!(range._1 <= weight && weight <= range._2)) 
          if (!g.selected(i)) removeMe += i 
    }  
    val h = g.remove(removeMe)
    h + ("activity" -> Metrics.activity(h,g))
  }

  /**
   * Filter the Edge weights
   */
  def edgeWeight(g: Graph): Graph = {
    if (g.nbNodes == 0) return g
    val range = Maths.map(
      g.get[(Double, Double)]("filter.edge.weight"),
      (0.0, 1.0),
      (g.get[Double]("minEdgeWeight"), g.get[Double]("maxEdgeWeight")))
    //println("applyEdgeWeight: " + range + " (" + g.get[(Double, Double)]("filter.edge.weight") + ")")
    val newLinks = g.links.zipWithIndex map {
      case (links,i) => 
        links.filter { 
          case (j, weight) => 
            if (!g.category(i).equalsIgnoreCase(g.category(j))) {
              true
            } else {
              //if (g.hasThisLink(j,i))
              //  true // always keep mutual links
              // else 
              (range._1 <= weight && weight <= range._2) // else we filter
            }
        }
    }
    val h = g + ("links" -> newLinks)
    h + ("activity" -> Metrics.activity(h,g))
  }
  
  def weightToSize(g: Graph) : Graph = {
    if (g.nbNodes == 0) return g
    val ratio = 1.0 * g.get[Double]("filter.node.size")
    val minmaxweight =  (g.get[Double]("minNodeWeight"), g.get[Double]("maxNodeWeight"))
    //println("applyWeightToSize: " + ratio)
    val newSize = g.weight map {
      case weight => Maths.map(weight,minmaxweight,(1.0, 50.0)) * ratio
    }
    g + ("size" -> newSize)
  }
}
