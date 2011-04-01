/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import eu.tinasoft.tinaviz.util.Maths

object Filters {


  def category(g:Graph) : Graph = {
    //println("applyCategory: "+g.debugStats)
    if (g.nbNodes == 0) return g
    var removeMe = Set.empty[Int]
    val category = g.currentCategory
    g.currentView match {
      case "macro" =>
        g.category.zipWithIndex map {
          case (cat, i) =>
            if (!g.currentCategory.equalsIgnoreCase(cat)) {
              removeMe += i
            }
        }

      case "meso" =>
        //println("\n\nfiltering the meso view: "+g.debugStats)
        g.selected.zipWithIndex foreach {
          case (f, i) => if (!f) {

              // remove the node which is not in our category
              if (!g.currentCategory.equalsIgnoreCase(g.category(i))) {
                removeMe += i
              } else {
                var keepThat = false
                // we remove nodes not connected to the selection
                g.selection.foreach {
                  case j => if (g.hasAnyLink(i, j)) keepThat = true
                }
                if (!keepThat) removeMe += i
              }
            }
        }
    }
    g.remove(removeMe)

  }
  /**
   * Filter the Nodes weights
   */
  def nodeWeight2(g: Graph): Graph = {
    if (g.nbNodes == 0) return g
    val rangeA = Maths.map(
      g.get[(Double, Double)]("filter.a.node.weight"),
      (0.0, 1.0),
      (g.minANodeWeight, g.maxANodeWeight))
    val rangeB = Maths.map(
      g.get[(Double, Double)]("filter.b.node.weight"),
      (0.0, 1.0),
      (g.minBNodeWeight, g.maxBNodeWeight))
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
    g.remove(removeMe)
    // h + ("activity" -> Metrics.activity(h,g))
  }
    /**
   * Filter the Nodes weights
   */
  def nodeWeight(g: Graph): Set[Int] = {
    if (g.nbNodes == 0) return Set.empty[Int]
    val rangeA = Maths.map(
      g.get[(Double, Double)]("filter.a.node.weight"),
      (0.0, 1.0),
      (g.minANodeWeight, g.maxANodeWeight))
    val rangeB = Maths.map(
      g.get[(Double, Double)]("filter.b.node.weight"),
      (0.0, 1.0),
      (g.minBNodeWeight, g.maxBNodeWeight))
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
    // h + ("activity" -> Metrics.activity(h,g))
  }
  /**
   * Filter the Edge weights
   */
  def edgeWeight(g: Graph): Graph = {
    if (g.nbNodes == 0) return g
    val arange = Maths.map(
      g.get[(Double, Double)]("filter.a.edge.weight"),
      (0.0, 1.0),
      (g.minAEdgeWeight, g.maxAEdgeWeight))
    val brange = Maths.map(
      g.get[(Double, Double)]("filter.b.edge.weight"),
      (0.0, 1.0),
      (g.minBEdgeWeight, g.maxBEdgeWeight))

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
              g.category(i) match {
                case "NGram" => (brange._1 <= weight && weight <= brange._2)
                case "Document" => (arange._1 <= weight && weight <= arange._2)
              }

            }
        }
    }
    val h = clean(g + ("links" -> newLinks))

    h// + ("activity" -> Metrics.activity(h,g))
  }
  
  def weightToSize(g: Graph) : Graph = {
    if (g.nbNodes == 0) return g
    val sliderRange = (3.0, 30.0) //node size range

    // small adjustement ("hack") to fit sliders to data from TinasoftPytextminer
    val aratio = 0.35 * g.get[Double]("filter.a.node.size") // Document
    val bratio = 1.0 * g.get[Double]("filter.b.node.size") // NGram

    val aminmaxweight =  (g.minANodeWeight, g.maxANodeWeight)  // Document
    val bminmaxweight =  (g.minBNodeWeight, g.maxBNodeWeight)  // NGram
    //println("applyWeightToSize: " + ratio)
    val newSize = g.weight.zipWithIndex map {
      case (weight,i) => Maths.map(weight, g.category(i) match {
                                      case "Document" => aminmaxweight
                                      case "NGram" => bminmaxweight
                                    //
                                   }, sliderRange) * (g.category(i) match {
                                      case "Document" => aratio
                                      case "NGram" => bratio
                                   })
    }
    g + ("size" -> newSize)
  }

      /**
   * Do some pre-processing, then send the final scene to the View
   * TODO: keep the Graph?
   */
  def clean(g:Graph) = {
    // TODO: do that in another Actor, which will reply directly to our master
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
