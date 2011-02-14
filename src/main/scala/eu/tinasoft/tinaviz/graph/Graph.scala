/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import eu.tinasoft._
import eu.tinasoft.tinaviz.util.Color
import collection.mutable.LinkedList
import eu.tinasoft.tinaviz.io.json.Base64
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
    "pause" -> false,
    "uuid" -> Array.empty[String],
    "label" -> Array.empty[String],
    "color" -> Array.empty[Color],
    "selected" -> Array.empty[Boolean],
    "density" -> Array.empty[Double],
    "rate" -> Array.empty[Int],
    "size" -> Array.empty[Double],
    "weight" -> Array.empty[Double],
    "category" -> Array.empty[String],
     "content" -> Array.empty[String],
    "position" -> Array.empty[(Double, Double)],
    "links" -> Array.empty[Map[Int, Double]],
    "inDegree" -> Array.empty[Int],
    "outDegree" -> Array.empty[Int],
    "nbNodes" -> 0,
    "nbEdges" -> 0,
    "camera.zoom" -> 1.0,
    "camera.position" -> (0.0,0.0),
    "filter.node.category" -> "Document",
    "filter.view" -> "macro",
    "selectionRadius" -> 10.0,
    "filter.node.size" -> 0.2,
    "filter.node.weight" -> (0.0, 1.0),
    "filter.edge.weight" -> (0.0, 1.0),
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
    "maxEdgeWeight" -> 0.0,
    "activity" -> 0.0,
    "entropy" -> 0.95

  )
}

class Graph(val _elements: Map[String, Any] = Map[String, Any]()) {

  val elements = Graph.defaults ++ _elements

  /**
   * Used for export to GEXF
   */
  def id(_uuid: String): Int = uuid.indexOf(_uuid)

  def getUuid(i: Int) = uuid(i)

  def has(_uuid: String): Boolean = uuid.contains(_uuid)

  def get[T](key: String): T = elements(key).asInstanceOf[T]

  def getArray[T](key: String): Array[T] = get[Array[T]](key)

  // some built-in functions
  lazy val links = getArray[Map[Int, Double]]("links")
  lazy val position = getArray[(Double, Double)]("position")
  lazy val color = getArray[Color]("color")
  lazy val weight = getArray[Double]("weight")
  lazy val size = getArray[Double]("size")
  lazy val category = getArray[String]("category")
  lazy val content = getArray[String]("content")
  lazy val selected = getArray[Boolean]("selected")
  lazy val label = getArray[String]("label")
  lazy val rate = getArray[Int]("rate")
  lazy val uuid = getArray[String]("uuid")
  lazy val inDegree = getArray[Int]("inDegree")
  lazy val outDegree = getArray[Int]("outDegree")
  lazy val density = getArray[Double]("density")
  lazy val nbNodes = get[Int]("nbNodes")
  lazy val nbEdges = get[Int]("nbEdges")
  lazy val entropy = get[Double]("entropy")
  lazy val activity = get[Double]("activity")
  lazy val pause = get[Boolean]("pause")
  lazy val ids = 0 until nbNodes
  lazy val cameraZoom = get[Double]("camera.zoom")
  lazy val cameraPosition = get[(Double,Double)]("camera.position")
  lazy val currentCategory = get[String]("filter.node.category")
  lazy val currentView = get[String]("filter.view")

  lazy val debugStats = {
    "**DEBUG**\nlinks.size: "+links.size+"\nposition.size: "+position.size+"\ncolor.size: "+color.size+"\nuuid.size: "+uuid.size+"\ncategory.size: "+category.size+"\nselected.size: "+selected.size+"\nselection.size: "+selection.size+"\n**END DEBUG**"
  }

  /**
   * Check if a graph has any link between i and i (directed or undirected)
   */
  def hasAnyLink(i: Int, j: Int) = hasThisLink(i, j) | hasThisLink(j, i)

  /**
   * Check if a graph has a directed link (from i to j)
   */
  def hasThisLink(i: Int, j: Int) = if (links.size > i) links(i).contains(j) else false

  /**
   * Create a new Graph with an updated column
   */
  def +(kv: (String, Any)) = set(kv)

  def +(id: Int, k: String, v: Any) = set(id, k, v)

  def ++(kv: Map[String, Any]) = new Graph(elements ++ kv)
  /**
    * Set a column and create a new Graph
    */
  def set(kv: (String, Any)) = new Graph(elements + kv)

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


    /**
     * List of selected nodes' IDs
     */
    lazy val selection : List[Int] = {
      val l = selected.zipWithIndex.filter { case (selected,i) => selected }.map{ case (s,i) => i }.toList
      println("******selection: "+l)
      l
    }

    /**
     * List of selected nodes' attributes
     */
    lazy val selectionAttributes = {
      //println("mapping selection attributes: "+selection)
      selection.map{ case i => lessAttributes(i) }.toList
    }

  /**
   * Return the current selection as a list of UUID:String
   */
  lazy val selectionUUID = selection.map{case i => getUuid(i)}.toList

  // { UUID : {neighbours}, UUID2; {neighbours}, ... }
   lazy  val selectionNeighbours = {
      Map(selectionUUID.zipWithIndex:_*).map{ case (uuid,i) => (uuid, neighbours(i)) }
    }

  /**
   * Get attributes of a node from it's UUID (Unique ID, arbitrary-length String)
   */
  def attributes(uuid: String): Map[String, Any] = {
     attributes(id(uuid))
  }


  /**
   * Get attributes of a node from it's index in the graph
   */
  def attributes(i: Int): Map[String, Any] = {
    Map[String, Any](
      "links"      -> (if (links.size > i) links(i) else Map.empty[Int,Double]),
      "position"   -> (if (position.size > i) position(i) else (0.0,0.0)),
      "color"      -> (if (color.size > i) color(i) else new Color(0.0,0.0,0.0)),
      "weight"     -> (if (weight.size > i) weight(i) else 1.0),
      "size"       -> (if (size.size > i) size(i) else 1.0),
      "category"   -> (if (category.size > i) category(i) else ""),
      "content"    -> (if (content.size > i) content(i) else ""),//Base64.encode(content(i)),
      "selected"   -> (if (selected.size > i) selected(i) else false),
      "label"      -> (if (label.size > i) label(i) else ""),// Base64.encode(label(i)),
      "rate"       -> (if (rate.size > i) rate(i) else 0),
      "id"         -> (if (uuid.size > i) uuid(i) else 0),
      "inDegree"   -> (if (inDegree.size > i) inDegree(i) else 0),
      "outDegree"  -> (if(outDegree.size > i) outDegree(i) else 0),
      "density"    -> (if (density.size > i) density(i) else 0)
    )
  }


  /**
   * Return neighbours of a node ID
   *
   */
  def neighbours(i:Int) : Map[String,Map[String,Any]] = {
      (if (links.size > i) {
        // println("  - mapping neighbours of node "+i+"..")
        links(i).map{ case (i,w) =>
          (getUuid(i),minimalAttributes(i)) }
      }
       else
        Map.empty[String,Map[String,Any]])
  }

  /**
   * Get "less" attributes (only the most important, for data transfert and talking with the visualization client)
   * of a node from it's UUID (Unique ID, arbitrary-length String)
   */
     def lessAttributes(uuid: String): Map[String, Any] = {
       lessAttributes(id(uuid))
  }

   /**
   * Get "less" attributes (only the most important, for data transfert and talking with the visualization client)
   * of a node from it's index in the graph
   */
  def lessAttributes(i: Int): Map[String, Any] = {
    /*
    println("***category: "+category.size)
     println("***selected: "+selected.size)
    println("***content: "+content.size)
    */
    Map[String, Any](
      //"links" -> links(i),
      //"position" -> position(i),
      //"color" -> color(i),
      //"weight" -> weight(i),
      //"size" -> size(i),
      "category" -> (if (category.size > i) category(i) else ""),
      "content" -> (if (content.size > i) content(i) else ""),//Base64.encode(content(i)),
      "selected" -> (if (selected.size > i) selected(i) else false),
      "label" -> (if (label.size > i) label(i) else ""),// Base64.encode(label(i)),
      "rate" -> (if (rate.size > i) rate(i) else 0),
      "id" -> (if (uuid.size > i) uuid(i) else 0),
      "inDegree" -> (if (inDegree.size > i) inDegree(i) else 0),
      "outDegree" -> (if(outDegree.size > i) outDegree(i) else 0)
      //"density" -> density(i)
    )
  }
     /**
   * Get "mininal" attributes (only the most important, for neighbourhood data)
   * of a node from it's index in the graph
   */
  def minimalAttributes(i: Int): Map[String, Any] = {
    /*
    println("***category: "+category.size)
     println("***selected: "+selected.size)
    println("***content: "+content.size)
    */
    Map[String, Any](
      //"links" -> links(i),
      //"position" -> position(i),
      //"color" -> color(i),
      //"weight" -> weight(i),
      //"size" -> size(i),
      "category" -> (if (category.size > i) category(i) else ""),
      //"content" -> (if (content.size > i) content(i) else ""),//Base64.encode(content(i)),
      // "selected" -> (if (selected.size > i) selected(i) else false),
      "label" -> (if (label.size > i) label(i) else ""),// Base64.encode(label(i)),
      //"rate" -> (if (rate.size > i) rate(i) else 0),
      "id" -> (if (uuid.size > i) uuid(i) else 0),
      "inDegree" -> (if (inDegree.size > i) inDegree(i) else 0),
      "outDegree" -> (if(outDegree.size > i) outDegree(i) else 0)
      //"density" -> density(i)
    )
  }
  /**
   * Get the map of all nodes
   */
  def allNodes: Map[String, Map[String,Any]] = {
    var nodeData = Map.empty[String,Map[String,Any]]
    for (i <- ids) nodeData += getUuid(i) -> lessAttributes(i)
    nodeData
  }

  /**
  var nodeData = Map(uuid.map {case uuid => (uuid,Map.empty[String,Any])}:_*)
  for (i <- ids) {
      //arrays(i).map{case (k,v) (k,v(i))}
      //val u = getUuid(i)
      //for ((k,v) <- arrays(i)) {
        // (k,v(i))
        //
         //nodeData(u) += k -> v(i)
      }
     //  nodeData(getUuid(i)) += (k,v(i))
    }
    arrays.foreach{
      case (k,v) =>
        nodeData += (k,v)
    }
    nodeData
  }*/

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

  /**
   * Return a new graph which is centered
   */
  def recenter = {
    var newCameraPosition = get[(Double,Double)]("camera.position")
     this + ("camera.position" -> newCameraPosition)
  }

  /**
   * compute the amount of information added to thiq by g
   */
  def computeActivity(g: Graph): Graph = {
    def max(x: Double, y: Double): Double = if (x < y) y else x
    var addNodes = 0.0
    var deletedNodes = 0.0
    var addEdges = 0.0
    var deletedEdges = 0.0
    g.uuid.zipWithIndex foreach {
      case (u, i) => if (!has(u)) addNodes += 1.0
    }
    uuid.zipWithIndex foreach {
      case (u, i) => if (!g.has(u)) deletedNodes += 1.0
    }
    val activity1 = activity * entropy
    val count = nbNodes + g.nbNodes
    val activity2 = if (count > 0) ((addNodes + deletedNodes) / count) else 0
    //val activity2 = if (count > 0) Maths.map(((addNodes + deletedNodes) / count),(0.0,1.0),(0.1,0.99)) else 0
    val a = max(activity1, activity2)
    //println("activity: " + a)
    this + ("activity" -> a)
  }

  def computeNbSingles = {
    var s = 0;
    links.foreach {
      case links => if (links.size == 0) s += 1
    }
    this + ("nbSingles" -> s)
  }

  def computeNbEdges = {
    var s = 0;
    links.foreach {
      case links => s += links.size
    }
    this + ("nbEdges" -> s)
  }

  def computeNbNodes = new Graph(elements + ("nbNodes" -> uuid.size))

  def computeOutDegree: Graph = {
    val _outDegree = links.map {
      case linkMap => linkMap.size
    }
    this + ("outDegree" -> _outDegree.toArray)
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
    this + ("inDegree" -> _inDegree.toArray)
  }

  def computeOutDegreeExtremums: Graph = {
    if (links.size == 0)
      return new Graph(elements ++ Map[String, Any]("minOutDegree" -> 0, "maxOutDegree" -> 0))
    var max = Int.MinValue
    var min = Int.MaxValue
    links.foreach {
      case n =>
        val d = n.size
        if (d < min) min = d
        if (d > max) max = d
    }
   this ++ Map[String,Any]("minOutDegree" -> min, "maxOutDegree" -> max)
  }

  def computeInDegreeExtremums: Graph = {
    if (links.size == 0)
      return this ++ Map[String, Any]("minOutDegree" -> 0, "maxOutDegree" -> 0)
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
    this ++ Map[String,Any]("minOutDegree" -> min, "maxOutDegree" -> max)
  }

  def computeExtremums: Graph = {
    this ++ (
    if (position.size == 0) {
       Map[String, Any]("xMax" -> 0.0, "xMin" -> 0.0, "yMax" -> 0.0, "yMin" -> 0.0)
    } else {
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
    Map[String, Any]("xMax" -> xMax,
      "xMin" -> xMin,
      "yMax" -> yMax,
      "yMin" -> yMin)
    })
  }


  def computeNodeWeightExtremums: Graph = {
    this ++ (
    if (position.size == 0) {
       Map[String, Any]("minNodeWeight" -> 0.0, "maxNodeWeight" -> 0.0)
    } else {
    var max = Double.MinValue
    var min = Double.MaxValue
    weight.foreach {
      case x =>
        if (x < min) min = x
        if (x > max) max = x
    }
     Map[String, Any]("minNodeWeight" -> min, "maxNodeWeight" -> max)
    })
  }

  def computeEdgeWeightExtremums: Graph = {
    this ++ (
    if (links.size == 0)  {
      Map[String, Any]("minEdgeWeight" -> 0.0, "maxEdgeWeight" -> 0.0)
    } else {
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
    Map[String, Any]("minEdgeWeight" -> min, "maxEdgeWeight" -> max)
    })

  }

  def computeBaryCenter: Graph = {
    var p = (0.0, 0.0)
    val N = position.size.toDouble
    position.foreach {
      case (x, y) => p = (p._1 + x, p._2 + y)
    }
    this + ("baryCenter" -> (if (N != 0) (p._1 / N, p._2 / N) else (0.0, 0.0)))
  }

  def map[T](id: Int, column: String, filter: T => T): Graph = {
    set(id, column, filter(getArray[T](column)(id)))
  }


  def map[T](column: String, filter: T => T): Graph = {
    this + (column -> (getArray[T](column).map {
      f => filter(f)
    }))
  }

  def filterNodeVisible[T](column: String, filter: T => Boolean) = {
    this + ( "visible" -> getArray[T](column).map { x => filter(x) })
  }

  def _filterNodeVisible[T](column: String, filter: T => Boolean) = {
     this + ( "visible" -> getArray[T](column).map {  x => filter(x) })
  }

  def converter(removed: Set[Int]): Array[Int] = {
    val _removed = removed.toList.sort {
      (a, b) => a < b
    }
    var j = 0
    (for (i <- ids) yield {
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

    val tmp1: Array[(Double, Double)] = position.zipWithIndex.map {
      case (elem, i) =>
        val u = uuid(i)
        val id = g.id(u)
        if (id == -1) elem else g.position(id)
    }.toArray

    val tmp2: Array[Boolean] = selected.zipWithIndex.map {
      case (s, i) =>
        val u = uuid(i)
        val id = g.id(u)
        if (id == -1) s else g.selected(id)
    }.toArray

    Graph.make(elements ++ Map[String,Any](
      "position" -> tmp1,
      "selected"  -> tmp2) // need to recompute things
    )
  }


}
