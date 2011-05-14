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

object Metrics {


  /**
   * Compute the number of single nodes
   */
  def nbSingles(g: Graph): Int = {
    var s = 0
    g.links.foreach {
      case lnk => if (lnk.size == 0) s += 1
    }
    s
  }


  /**
   * Compute the number of edges
   */
  def nbEdges(g: Graph): Int = {
    var s = 0
    g.links.foreach {
      case lnk => s += lnk.size
    }
    s
  }

  /**
   * Compute the number of nodes
   */
  def nbNodes(g: Graph): Int = g.uuid.size

  /**
   * Compute the out degree array
   */
  def outDegree(g: Graph): Array[Int] = {
    val _outDegree = g.links.map {
      case m => m.size
    }
    _outDegree.toArray
  }

  /**
   * Compute the in degree array
   */
  def inDegree(g: Graph): Array[Int] = {

    val _inDegree = g.uuid.zipWithIndex.map {
      case (n, i) =>
        var d = 0
        g.links foreach {
          case m => if (m.contains(i)) d += 1
        }
        d
    }
    _inDegree.toArray
  }
  
  
  def connectedComponents(g:Graph) : List[Int] = {
      
    // Calcul des partitions
    var nb_partition = 0

    //var nodesId = Map(g.ids.zipWithIndex: _*).map {
    //  case (id, id) => (id,id)
    //}

    var nodesIds : Set[Int] = g.ids.toSet

    var partitions = Map.empty[Int,Int]

    while (nodesIds.size > 0) {
        val target = nodesIds.head
        //nodesIds /: remove(0)
        nodesIds = nodesIds - target
        nb_partition += 1
        var current_partition = Set( target )
        partitions += target -> nb_partition

        // neighbors IDs
        val neighborsList = g.neighbours( target ).map{ case (a,_) => g.id(a) }.toList
        
        var neighbors =  ( neighborsList.toSet --  current_partition.toSet )

        while (neighbors.size > 0) {
            val target2 = neighbors.head

            val neighbors2 = g.neighbours( target2 ).map{ case (a,b) => g.id(a) }.toSet

            partitions += target2 -> nb_partition

            neighbors = neighbors - target2 // only keep the last elements except the first
            nodesIds = nodesIds - target2 // remove target2 from nodesIds

            current_partition += target2 // append target2 to current_parititon

            // do the union of neighbors 1 and 2, then compute the difference with current partition
            neighbors = (neighbors | neighbors2) &~ current_partition
        }
    }
   println("number of partitions"+nb_partition) 
    // sort the Map of ( ID -> PARTITION ) then only keep the partition's number'
    partitions.toList sortBy {_._1} map { _._2 } 
  }
  
  

  /**
   * Compute the degree array
   */
  def degree(g: Graph): Array[Int] = {
    val _degree = g.links.zipWithIndex.map {
      case (aLinks, a) =>
      // var d = 0
        var neighbourMap = Map.empty[Int, Boolean]
        aLinks.foreach {
          case (b, w) =>
            if (b != a) {
              val tpl: (Int, Boolean) = (b, true)
              neighbourMap += tpl
            }

        }


        g.links.zipWithIndex.map {
          case (a2Links, a2) =>
            a2Links.foreach {
              case (b, w) =>
                if (b == a && a != a2) {
                  // inlink!
                  val tpl: (Int, Boolean) = (a2, true)
                  neighbourMap += tpl
                }
            }
        }
        neighbourMap.size
    }
    _degree.toArray
  }

  /**
   * Compute the out degree extremums
   */
  def outDegreeExtremums(g: Graph): (Int, Int) = {
    if (g.links.size == 0) {
      (0, 0)
    } else {
      var max = Int.MinValue
      var min = Int.MaxValue
      g.links foreach {
        case n =>
          val d = n.size
          if (d < min) min = d
          if (d > max) max = d
      }
      (min, max)
    }
  }

  /**
   * Compute the in degree extremums
   */
  def inDegreeExtremums(g: Graph): (Int, Int) = {
    if (g.links.size == 0) {
      (0, 0)
    } else {
      var max = Int.MinValue
      var min = Int.MaxValue

      g.ids foreach {
        case id =>
          var d = 0
          g.links foreach {
            case m => if (m.contains(id)) d += 1
          }
          if (d < min) min = d
          if (d > max) max = d
      }
      (min, max)
    }
  }

  /**
   * Compute the extremums (X min, X max, Y min, Y max)
   */
  def extremums(g: Graph): (Double, Double, Double, Double) = {
    if (g.position.size == 0) {
      (0.0, 0.0, 0.0, 0.0)
    } else {
      var xMax = Double.MinValue
      var xMin = Double.MaxValue
      var yMax = Double.MinValue
      var yMin = Double.MaxValue
      g.position foreach {
        case (x, y) =>
          if (x < xMin) xMin = x
          if (x > xMax) xMax = x
          if (y < yMin) yMin = y
          if (y > yMax) yMax = y
      }
      (xMax, xMin, yMax, yMin)
    }
  }

  /**
   * Compute the extremums (X min, X max, Y min, Y max)
   */
  def extremumsSelection(g: Graph): (Double, Double, Double, Double) = {
    if (g.position.size == 0) {
      (0.0, 0.0, 0.0, 0.0)
    } else {
      val _selectedNodes = g.position.zipWithIndex filter {
        case (p, i) => g.selected(i)
      }
      if (_selectedNodes.size == 0) {
        (0.0, 0.0, 0.0, 0.0)
      } else {
        var xMax = Double.MinValue
        var xMin = Double.MaxValue
        var yMax = Double.MinValue
        var yMin = Double.MaxValue
        _selectedNodes foreach {
          case ((x, y), i) =>
            if (x < xMin) xMin = x
            if (x > xMax) xMax = x
            if (y < yMin) yMin = y
            if (y > yMax) yMax = y
        }
        (xMax, xMin, yMax, yMin)
      }
    }
  }

  // a list of positions + ID
  def selectionNeighbourhood(g:Graph) = {
     val tmp = g.position.zipWithIndex filter {
        case (p, i) => g.selected(i)
     }
     g.position.zipWithIndex filter {
        case (p, i1) =>
             tmp.find {
               case (p2, i2) =>
                 (g.hasAnyLink(i1,i2) || i1 == i2)
             } match {
                 case None => false
                 case any => true
             }
      }
  }

    def selectionNeighbourhoodCenter(g:Graph) : (Double, Double) = {
    var p = (0.0, 0.0)
    val N = g.selectionNeighbourhood.size.toDouble
    g.selectionNeighbourhood foreach {
      case ((x, y), i) => p = (p._1 + x, p._2 + y)
    }
    if (N != 0) (p._1 / N, p._2 / N) else (0.0, 0.0)
    }

  /**
   * Compute the extremums (X min, X max, Y min, Y max)
   */
  def extremumsSelectionNeighbourhood(g: Graph): (Double, Double, Double, Double) = {
    if (g.position.size == 0) {
      (0.0, 0.0, 0.0, 0.0)
    } else {

      if (g.selectionNeighbourhood.size == 0) {
        (0.0, 0.0, 0.0, 0.0)
      } else {
        var xMax = Double.MinValue
        var xMin = Double.MaxValue
        var yMax = Double.MinValue
        var yMin = Double.MaxValue
        g.selectionNeighbourhood foreach {
          case ((x, y), i) =>
            if (x < xMin) xMin = x
            if (x > xMax) xMax = x
            if (y < yMin) yMin = y
            if (y > yMax) yMax = y
        }
        (xMax, xMin, yMax, yMin)
      }
    }
  }

  def notSingleNodesDimension(g: Graph): (Double, Double) = {
    if (g.position.size == 0) {
      (0.0, 0.0)
    } else {
      val notSingles = g.position.zipWithIndex filter {
        case (pos, i) => !g.isSingle(i)
      }
      if (notSingles.size == 0.0) {
        (0.0, 0.0)
      } else {
        var xMax = Double.MinValue
        var xMin = Double.MaxValue
        var yMax = Double.MinValue
        var yMin = Double.MaxValue
        notSingles foreach {
          case ((x, y), i) =>
            if (x < xMin) xMin = x
            if (x > xMax) xMax = x
            if (y < yMin) yMin = y
            if (y > yMax) yMax = y
        }
        (abs(xMax - xMin), abs(yMax - yMin))
      }
    }
  }

  /**
   * Compute the node weight extremums (min node weight, max node weight)
   * return a Tuple of Double
   */
  def nodeWeightExtremums(g: Graph): (Double, Double, Double, Double) = {
    if (g.weight.size == 0) {
      (0.0, 0.0, 0.0, 0.0)
    } else {
      var amax = Double.MinValue
      var amin = Double.MaxValue
      var bmax = Double.MinValue
      var bmin = Double.MaxValue
      g.weight.zipWithIndex foreach {
        case (x, i) =>
          g.category(i) match {
            case "NGram" =>
              if (x < bmin) bmin = x
              if (x > bmax) bmax = x
            case "Document" =>
              if (x < amin) amin = x
              if (x > amax) amax = x
          }

      }
      (amin, amax, bmin, bmax)
    }
  }

  /**
   * Compute the edge weight extremums (min edge weight, max edge weight)
   * return a Tuple of Double
   */
  def edgeWeightExtremums(g: Graph): (Double, Double, Double, Double) = {
    if (g.links.size == 0) {
      (0.0, 0.0, 0.0, 0.0)
    } else {
      var amax = Double.MinValue
      var amin = Double.MaxValue
      var bmax = Double.MinValue
      var bmin = Double.MaxValue
      g.links.zipWithIndex foreach {
        case (lnks, i) =>
          lnks.foreach {
            case (id, weight) =>
              g.category(i) match {
                case "NGram" =>
                  if (weight < bmin) bmin = weight
                  if (weight > bmax) bmax = weight
                case "Document" =>
                  if (weight < amin) amin = weight
                  if (weight > amax) amax = weight
              }
          }
      }
      (amin, amax, bmin, bmax)
    }
  }

  /**
   * Compute a graph's center. Very simple - you probably don't want to use that
   */
  def basicCenter(g: Graph): (Double, Double) = {
    val (xmin, xmax, ymin, ymax) = g.extremums
    ((xmax - xmin),(ymax - ymax))
  }


  /**
   * Compute a graph's barycenter
   */
  def baryCenter(g: Graph): (Double, Double) = {
    var p = (0.0, 0.0)
    val N = g.position.size.toDouble
    g.position foreach {
      case (x, y) => p = (p._1 + x, p._2 + y)
    }
    if (N != 0) (p._1 / N, p._2 / N) else (0.0, 0.0)
  }

  /**
   * Compute a graph's selection center
   */
  def selectionCenter(g: Graph): (Double, Double) = {
    var p = (0.0, 0.0)
    val _selectedNodes = g.position.zipWithIndex filter {
      case (p, i) => g.selected(i)
    }
    val N = _selectedNodes.size.toDouble
    _selectedNodes foreach {
      case ((x, y), i) => p = (p._1 + x, p._2 + y)
    }
    if (N != 0) (p._1 / N, p._2 / N) else (0.0, 0.0)
  }

  /**
   * Compute a graph's bary center, taking only single nodes in account
   */
  def singlesCenter(g: Graph): (Double, Double) = {
    var p = (0.0, 0.0)
    val singles = g.position.zipWithIndex filter {
      case (p, i) => g.isSingle(i)
    }
    val N = singles.size.toDouble
    singles foreach {
      case ((x, y), i) => p = (p._1 + x, p._2 + y)
    }
    if (N != 0) (p._1 / N, p._2 / N) else (0.0, 0.0)
  }

  /**
   * Compute a graph's bary center, taking only nodes not singles in account
   */
  def notSinglesCenter(g: Graph): (Double, Double) = {
    var p = (0.0, 0.0)
    val notSingles = g.position.zipWithIndex filter {
      case (p, i) => !g.isSingle(i)
    }
    val N = notSingles.size.toDouble
    notSingles foreach {
      case ((x, y), i) => p = (p._1 + x, p._2 + y)
    }
    if (N != 0) (p._1 / N, p._2 / N) else (0.0, 0.0)
  }
}