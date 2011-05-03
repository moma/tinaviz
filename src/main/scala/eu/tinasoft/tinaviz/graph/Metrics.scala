/************************************************************************
                                  Tinaviz
*************************************************************************
 This application is part of the Tinasoft project: http://tinasoft.eu
 Tinaviz main developer: julian.bilcke @ iscpif.fr  (twitter.com/flngr)

Copyright (C) 2009-2011 CREA Lab, CNRS/Ecole Polytechnique UMR 7656 (Fr)
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