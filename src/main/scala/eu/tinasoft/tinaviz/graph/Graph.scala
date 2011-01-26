/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import eu.tinasoft._
import eu.tinasoft.tinaviz.util.Color
import scala.collection.mutable.LinkedList
import tinaviz.util.Vector

object Graph {

  def get[T](elements: Map[String, Any], key: String): T = elements.get(key).get.asInstanceOf[T]


  // TODO be an optimized factory
  def makeDiff(newElements: Map[String, Array[Any]],
               oldElements: Map[String, Array[Any]]) = {

  }

  /**
   * Default, dumb factory
   */
  def make(elements: Map[String, Any]) = {
    new Graph(elements).computeAll
  }

  val defaults: Map[String, Any] = Map(
    "uuid" -> Array.empty[String],
    "label" -> Array.empty[String],
    "color" -> Array.empty[Color],
    "selected" -> Array.empty[Boolean],
    "density" -> Array.empty[Double],
    "rate" -> Array.empty[Int],
    "size" -> Array.empty[Double],
    "weight" -> Array.empty[Double],
    "category" -> Array.empty[String],
    "position" -> Array.empty[(Double, Double)],
    "links" -> Array.empty[Map[Int, Double]],
    "inDegree" -> Array.empty[Int],
    "outDegree" -> Array.empty[Int],
    "nbNodes" -> 0,
    "nbEdges" -> 0,
    "filter.node.category" -> "Document",
    "filter.node.size" -> 0.2,
    "xMin" -> 0,
    "yMin" -> 0,
    "xMax" -> 0,
    "yMax" -> 0,
    "minOutDegree" -> 0,
    "minInDegree" -> 0,
    "maxnOutDegree" -> 0,
    "maxInDegree" -> 0,
    "minNodeWeight" -> 0.0,
    "maxNodeWeight" -> 0.0,
    "minEdgeWeight" -> 0.0,
    "maxEdgeWeight" -> 0.0
  )
}

class Graph(val _elements: Map[String, Any] = Map[String, Any]()) {

  val elements = Graph.defaults ++ _elements

  /**
   * Used for export to GEXF
   */
  def id(_uuid: String): Int = uuid.indexOf(_uuid)

  def getUuuid(i: Int) = uuid(i)

  def get[T](key: String): T = elements(key).asInstanceOf[T]

  def getArray[T](key: String): Array[T] = get[Array[T]](key)

  // some built-in functions
  val links = getArray[Map[Int, Double]]("links")
  val position = getArray[(Double, Double)]("position")
  val color = getArray[Color]("color")
  val weight = getArray[Double]("weight")
  val category = getArray[String]("category")
  val selected = getArray[Boolean]("selected")
  val label = getArray[String]("label")
  val rate = getArray[Int]("rate")
  val uuid = getArray[String]("uuid")
  val inDegree = getArray[Int]("inDegree")
  val outDegree = getArray[Int]("outDegree")
  val density = getArray[Double]("density")
  val nbNodes = get[Int]("nbNodes")
  val nbEdges = get[Int]("nbEdges")
  val ids = 0 until nbNodes
  // a Range on ID

  def hasAnyLink(i: Int, j: Int) = hasThisLink(i, j) | hasThisLink(j, i)

  def hasThisLink(i: Int, j: Int) = if (links.size > i) links(i).contains(j) else false

  def +(kv: (String, Any)) = {
    new Graph(elements + kv)
  }

  def +(id: Int, k: String, v: Any) = {
    set(id, k, v)
  }

  def set(id: Int, k: String, value: Any) = {
    //println("id: "+id+" kv: "+kv)
    var newElements = elements
    newElements += k -> {
      if (!elements.contains(k)) {
        value match {
          case v: Boolean => List[Boolean](v).toArray
          case v: Int => List[Int](v).toArray
          case v: Double => List[Double](v).toArray
          case v: Float => List[Float](v).toArray
          case v: String => List[String](v).toArray
          case v: Color => List[Color](v).toArray
          case v: (Double, Double) => List[(Double, Double)](v).toArray
          case v: Array[Double] => List[Array[Double]](v).toArray
          case v: Array[Int] => List[Array[Int]](v).toArray
          case v: List[Double] => List[List[Double]](v).toArray
          case v: List[Int] => List[List[Int]](v).toArray
          case v: Set[Int] => List[Set[Int]](v).toArray
          case v: Map[Int, Double] => List[Map[Int, Double]](v).toArray
          case v =>
            throw new Exception("UNRECOGNIZED TYPE")
        // List(v).toArray
        }
      } else {
        //println("key "+k+" already match!")
        val t = elements(k)
        //println("elements gave "+t+" ")

        value match {
          case v: Boolean =>
            var m = getArray[Boolean](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Boolean](v)).toArray
            m
          case v: Int =>
            var m = getArray[Int](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Int](v)).toArray
            m
          case v: Double =>
            var m = getArray[Double](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Double](v)).toArray
            m
          case v: Float =>
            var m = getArray[Float](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Float](v)).toArray
            m
          case v: String =>
            var m = getArray[String](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[String](v)).toArray
            m
          case v: Color =>
            var m = getArray[Color](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Color](v)).toArray
            m
          case v: (Double, Double) =>
            var m = getArray[(Double, Double)](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[(Double, Double)](v)).toArray
            m
          case v: Map[Int, Double] =>
            var m = getArray[Map[Int, Double]](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Map[Int, Double]](v)).toArray
            m
          case v: List[Double] =>
            var m = getArray[List[Double]](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[List[Double]](v)).toArray
            m
          case v: Array[Double] =>
            var m = getArray[Array[Double]](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Array[Double]](v)).toArray
            m
          case v: List[Int] =>
            var m = getArray[List[Int]](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[List[Int]](v)).toArray
            m
          case v: Array[Int] =>
            var m = getArray[Array[Int]](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Array[Int]](v)).toArray
            m
          case v: Set[Int] =>
            var m = getArray[Set[Int]](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Set[Int]](v)).toArray
            m

          case v: Any =>
          // Actually, this is the only case called
            throw new Exception("FATAL FATAL FATAL, got any: " + v)
        }
      }
    }
    new Graph(newElements)
  }

  def set(kv: (String, Any)) = new Graph(elements + kv)

  def computeAll = {
    var g = this
    g = g.computeNbNodes
    g = g.computeNbEdges
    g = g.computeNbSingles
    g = g.computeOutDegree
    g = g.computeInDegree
    g = g.computeOutDegreeExtremums
    g = g.computeInDegreeExtremums
    g = g.computeExtremums
    g = g.computeNodeWeightExtremums
    g = g.computeEdgeWeightExtremums
    g.computeBaryCenter
  }

  def computeNbSingles = {
    var s = 0;
    links.foreach {
      case links => if (links.size == 0) s += 1
    }
    new Graph(elements + ("nbSingles" -> s))
  }

  def computeNbEdges = {
    var s = 0;
    links.foreach {
      case links => s += links.size
    }
    new Graph(elements + ("nbEdges" -> s))
  }

  def computeNbNodes = new Graph(elements + ("nbNodes" -> uuid.size))

  def computeOutDegree: Graph = {
    val _outDegree = links.map {
      case linkMap => linkMap.size
    }
    new Graph(elements + ("outDegree" -> _outDegree.toArray))
  }

  def computeInDegree: Graph = {

    val _inDegree = uuid.zipWithIndex.map {
      case (n, i) =>
        var d = 0
        links.foreach {
          case m => if (m.contains(i)) d += 1
        }
        d
    }
    new Graph(elements ++ Map[String, Any]("inDegree" -> _inDegree.toArray))
  }

  def computeOutDegreeExtremums: Graph = {
    if (links.size == 0)
      return new Graph(elements ++ Map[String, Any]("minOutDegree" -> 0,
        "maxOutDegree" -> 0))
    var max = Int.MinValue
    var min = Int.MaxValue
    links.foreach {
      case n =>
        val d = n.size
        if (d < min) min = d
        if (d > max) max = d
    }
    new Graph(elements ++ Map[String, Any]("minOutDegree" -> min,
      "maxOutDegree" -> max))
  }

  def computeInDegreeExtremums: Graph = {
    if (links.size == 0)
      return new Graph(elements ++ Map[String, Any]("minOutDegree" -> 0,
        "maxOutDegree" -> 0))
    var max = Int.MinValue
    var min = Int.MaxValue

    uuid.zipWithIndex foreach {
      case (_uuid, id) =>
        var d = 0
        links.foreach {
          case mapIntDouble => if (mapIntDouble.contains(id)) d += 1
        }
        if (d < min) min = d
        if (d > max) max = d
    }
    new Graph(elements ++ Map[String, Any]("minOutDegree" -> min,
      "maxOutDegree" -> max))
  }

  def computeExtremums: Graph = {
    if (position.size == 0)
      return new Graph(elements ++ Map[String, Any]("xMax" -> 0.0,
        "xMin" -> 0.0,
        "yMax" -> 0.0,
        "yMin" -> 0.0))
    var xMax = Double.MinValue
    var xMin = Double.MaxValue
    var yMax = Double.MinValue
    var yMin = Double.MaxValue
    position.foreach {
      case (x, y) =>
        if (x < xMin) xMin = x
        if (x > xMax) xMax = x
        if (y < yMin) yMin = y
        if (y > yMax) yMax = y
    }
    return new Graph(elements ++ Map[String, Any]("xMax" -> xMax,
      "xMin" -> xMin,
      "yMax" -> yMax,
      "yMin" -> yMin))
  }


  def computeNodeWeightExtremums: Graph = {
    if (position.size == 0)
      return new Graph(elements ++ Map[String, Any]("minNodeWeight" -> 0.0, "maxNodeWeight" -> 0.0))
    var max = Double.MinValue
    var min = Double.MaxValue
    weight.foreach {
      case x =>
        if (x < min) min = x
        if (x > max) max = x
    }
    return new Graph(elements ++ Map[String, Any]("minNodeWeight" -> min, "maxNodeWeight" -> max))
  }

  def computeEdgeWeightExtremums: Graph = {
    if (links.size == 0)
      return new Graph(elements ++ Map[String, Any]("minEdgeWeight" -> 0.0, "maxEdgeWeight" -> 0.0))
    var max = Double.MinValue
    var min = Double.MaxValue
    links.foreach {
      case lnks =>
        lnks.foreach {
          case (id, weight) =>
            if (weight < min) min = weight
            if (weight > max) max = weight
        }
    }
    return new Graph(elements ++ Map[String, Any]("minEdgeWeight" -> min, "maxEdgeWeight" -> max))
  }

  def computeBaryCenter: Graph = {
    var p = (0.0, 0.0)
    val N = position.size.toDouble
    position.foreach {
      case (x, y) => p = (p._1 + x, p._2 + y)
    }
    new Graph(elements ++ Map[String, Any]("baryCenter" -> (if (N != 0) (p._1 / N, p._2 / N) else (0.0, 0.0))))
  }

  def map[T](id: Int, column: String, filter: T => T): Graph = {
    set(id, column, filter(getArray[T](column)(id)))
  }


  def map[T](column: String, filter: T => T): Graph = {
    new Graph(elements ++ Map[String, Any](column -> (getArray[T](column).map {
      f => filter(f)
    })))
  }

  def filterNodeVisible[T](column: String, filter: T => Boolean) = {

    new Graph(elements ++ Map[String, Any](
      "visible" -> getArray[T](column).map {
        x => filter(x)
      }))
  }

  def _filterNodeVisible[T](column: String, filter: T => Boolean) = {

    new Graph(elements ++ Map[String, Any](
      "visible" -> getArray[T](column).map {
        x => filter(x)
      }))
  }

  def converter(removed: Set[Int]): Array[Int] = {
    val _removed = removed.toList.sort {
      (a, b) => a < b
    }
    var j = 0
    (for (i <- 0 until nbNodes) yield {
      (if (_removed.size > j && i == _removed(j)) {
        j += 1
        -1
      } else {
        i - j
      })
    }).toArray
  }

  def remove(set: Set[Int]): Graph = {
    val conv = converter(set)
    val newElements = elements.map {

      case ("links", entries: Array[Map[Int, Double]]) =>
        val filteredEntries = entries.zipWithIndex.filter {
          case (e, i) => conv(i) >= 0
        }.map(_._1).toArray
        val newEntries = filteredEntries.map {
          links =>
            links.filter {
              case (id, weight) => conv(id) >= 0
            }.map {
              case (id, weight) => (conv(id), weight)
            }
        }
        ("links", newEntries.toArray)

      case (key: String, entries: Array[Boolean]) =>
        (key, entries.zipWithIndex.filter {
          case (e, i) => conv(i) >= 0
        }.map(_._1).toArray)
      case (key: String, entries: Array[Double]) =>
        (key, entries.zipWithIndex.filter {
          case (e, i) => conv(i) >= 0
        }.map(_._1).toArray)
      case (key: String, entries: Array[Int]) =>
        (key, entries.zipWithIndex.filter {
          case (e, i) => conv(i) >= 0
        }.map(_._1).toArray)
      case (key: String, entries: Array[Color]) =>
        (key, entries.zipWithIndex.filter {
          case (e, i) => conv(i) >= 0
        }.map(_._1).toArray)
      case (key: String, entries: Array[(Double, Double)]) =>
        (key, entries.zipWithIndex.filter {
          case (e, i) => conv(i) >= 0
        }.map(_._1).toArray)
      case (key: String, entries: Array[String]) =>
        (key, entries.zipWithIndex.filter {
          case (e, i) => conv(i) >= 0
        }.map(_._1).toArray)
      case (key: String, entries: Array[Any]) =>
        (key, entries.zipWithIndex.filter {
          case (e, i) => conv(i) >= 0
        }.map(_._1).toArray)

      case (key, entry) => (key, entry)
    }

    Graph.make(newElements)
  }


  def updatePosition(g: Graph): Graph = {
    val source = g.getArray[(Double, Double)]("position")
    var target: Array[(Double, Double)] = getArray[(Double, Double)]("position").zipWithIndex.map {
      case (elem, i) =>
        val u = uuid(i)
        val id = g.id(u)
        if (id == -1) elem else source(id)
    }
    Graph.make(elements + ("position" -> target.toArray))
  }


}
