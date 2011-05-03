/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import eu.tinasoft._
import tinaviz.util._
import tinaviz.util.Color._
import tinaviz.util.Vector
import tinaviz.io.json.Base64
import collection.mutable.LinkedList


object Graph {

  def get[T](elements: Map[String, Any], key: String): T = elements.get(key).get.asInstanceOf[T]

  // TODO be an optimized factory
  def makeDiff(newElements: Map[String, Array[Any]],
               oldElements: Map[String, Array[Any]]) = {

  }

  /**
   * Default, dumb factory
   */
  def make(elements: Map[String, Any]) = new Graph(elements)

  /**
   * Default settings
   */
  val defaults: Map[String, Any] = Map(
    "pause" -> true,
    "uuid" -> Array.empty[String],
    "label" -> Array.empty[String],
    "color" -> Array.empty[Color],
    "selected" -> Array.empty[Boolean],
    "highlighted" -> Array.empty[Boolean],
    "updateStatus" -> Array.empty[Symbol], // outdated, updating, updated
    "saveStatus" -> Array.empty[Symbol], // saving, saved
    "density" -> Array.empty[Double],
    "rate" -> Array.empty[Int],
    "size" -> Array.empty[Double],
    "weight" -> Array.empty[Double],
    "category" -> Array.empty[String],
    "content" -> Array.empty[String],
    "position" -> Array.empty[(Double, Double)],
    "links" -> Array.empty[Map[Int, Double]],
    "camera.zoom" -> 1.0,
    "camera.position" -> (0.0, 0.0),
    "camera.target" -> "all", //'all, 'none, or 'selection
    "filter.node.category" -> "Document",
    "filter.view" -> "macro",
    "selectionRadius" -> 10.0,
    "filter.a.node.size" -> 0.2,
    "filter.a.node.weight" -> (0.0, 1.0),
    "filter.a.edge.weight" -> (0.0, 1.0),
    "filter.b.node.size" -> 0.2,
    "filter.b.node.weight" -> (0.0, 1.0),
    "filter.b.edge.weight" -> (0.0, 1.0),
    "filter.map.node.color.hue" -> "category",
    "filter.map.node.color.saturation" -> "weight",
    "filter.map.node.color.brightness" -> "weight",
    "filter.map.node.size" -> "weight",
    "filter.map.node.shape" -> "category",
    //"edge.type" -> "line",
    "layout" -> "tinaforce",  // phyloforce
    "activity" -> 100.0,
    "entropy" -> 0.95,
    "maxDrawedNodes" -> 10,
    "debug" -> false
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
  lazy val highlighted = getArray[Boolean]("highlighted")
  lazy val updateStatus = getArray[Symbol]("updateStatus")
  // outdated, updating, updated
  lazy val saveStatus = getArray[Symbol]("saveStatus")
  // saving, saved
  lazy val label = getArray[String]("label")
  lazy val rate = getArray[Int]("rate")
  lazy val uuid = getArray[String]("uuid")

  lazy val outDegree = Metrics outDegree this
  lazy val inDegree = Metrics inDegree this
  lazy val degree = Metrics degree this

  lazy val totalDegree = inDegree zip outDegree map {
    case (a, b) => a + b
  }
  lazy val density = getArray[Double]("density")

  lazy val ids = 0 until nbNodes

  // metrics  & properties
  lazy val nbNodes: Int = Metrics nbNodes this
  lazy val nbEdges: Int = Metrics nbEdges this
  lazy val nbSingles: Int = Metrics nbSingles this

  lazy val entropy = get[Double]("entropy")
  lazy val activity = get[Double]("activity")

  lazy val colorScheme = Rio

  // camera settings
  lazy val cameraZoom = get[Double]("camera.zoom")
  lazy val cameraPosition = get[(Double, Double)]("camera.position")
  lazy val cameraTarget = get[String]("camera.target")

  // filters and view settings
  lazy val currentCategory = get[String]("filter.node.category")
  lazy val currentView = get[String]("filter.view")
  lazy val layout = get[String]("layout")
  lazy val pause = get[Boolean]("pause")
  lazy val selectionRadius = get[Double]("selectionRadius")

  lazy val edgeType = get[String]("edge.type")

  lazy val baryCenter = Metrics baryCenter this
  lazy val selectionCenter = Metrics selectionCenter this

  // a list of positions + ID
  lazy val selectionNeighbourhood = Metrics selectionNeighbourhood this

  lazy val selectionNeighbourhoodCenter = Metrics selectionNeighbourhoodCenter this

  lazy val singlesCenter = Metrics singlesCenter this
  lazy val notSinglesCenter = Metrics notSinglesCenter this

  lazy val outDegreeExtremums = Metrics outDegreeExtremums this
  lazy val minOutDegree = outDegreeExtremums._1
  lazy val maxOutDegree = outDegreeExtremums._2

  lazy val inDegreeExtremums = Metrics inDegreeExtremums this
  lazy val minInDegree = inDegreeExtremums._1
  lazy val maxInDegree = inDegreeExtremums._2

  lazy val extremums = Metrics extremums this
  lazy val xMax = extremums._1
  lazy val xMin = extremums._2
  lazy val yMax = extremums._3
  lazy val yMin = extremums._4

  lazy val extremumsSelection = Metrics extremumsSelection this
  lazy val xMaxSelection = extremumsSelection._1
  lazy val xMinSelection = extremumsSelection._2
  lazy val yMaxSelection = extremumsSelection._3
  lazy val yMinSelection = extremumsSelection._4

  lazy val extremumsSelectionNeighbourhood = Metrics extremumsSelectionNeighbourhood this
  lazy val xMaxSelectionNeighbourhood = extremumsSelectionNeighbourhood._1
  lazy val xMinSelectionNeighbourhood = extremumsSelectionNeighbourhood._2
  lazy val yMaxSelectionNeighbourhood = extremumsSelectionNeighbourhood._3
  lazy val yMinSelectionNeighbourhood = extremumsSelectionNeighbourhood._4

  lazy val nodeWeightExtremums = Metrics nodeWeightExtremums this
  lazy val minANodeWeight = nodeWeightExtremums._1
  lazy val maxANodeWeight = nodeWeightExtremums._2
  lazy val minBNodeWeight = nodeWeightExtremums._3
  lazy val maxBNodeWeight = nodeWeightExtremums._4

  lazy val edgeWeightExtremums = Metrics edgeWeightExtremums this
  lazy val minAEdgeWeight = edgeWeightExtremums._1
  lazy val maxAEdgeWeight = edgeWeightExtremums._2
  lazy val minBEdgeWeight = edgeWeightExtremums._3
  lazy val maxBEdgeWeight = edgeWeightExtremums._4

  lazy val selectionValid = (selection.size > 0)

  lazy val renderNodeColor = {
    selected.zipWithIndex map {
      case (s, i) =>
        val mode = if (s) 'selected else if (highlighted(i)) 'highlighted else if (selectionValid) 'unselected else 'default
        val color = (category(i) match {
          case "Document" => colorScheme.primary
          case "NGram" => colorScheme.tertiary
          case other => colorScheme.secondary
        })
        mode match {
          case 'selected => color.darker.saturateBy(1.00)
          case 'highlighted => color.darker.saturateBy(0.85)
          case 'unselected => color.standard.saturateBy(0.80)
          case 'default => color.standard.saturateBy(0.90)
        }
    }
  }

  /**
   * Lazy computation of the node border
   */
  lazy val renderNodeBorderColor = {

    val darkerColor = new Color(0.0, 1.0, 0.0).alpha(0.8)

    selected.zipWithIndex map {
      case (s, i) =>
        val mode = if (s) 'selected else if (highlighted(i)) 'highlighted else if (selectionValid) 'unselected else 'default
        val color = (category(i) match {
          case "Document" => colorScheme.primary
          case "NGram" => colorScheme.tertiary
          case other => colorScheme.secondary
        })
        mode match {
          case 'selected => darkerColor
          case 'highlighted => darkerColor.saturateBy(0.90)
          case 'unselected => color.darker.saturation(0.90)
          case 'default => color.darker.saturation(1.0)
        }
    }
  }


  /**
   * Lazy computation of the edge size to screen
   */
  lazy val renderEdgeSize = {
    (for ((links, i) <- links.zipWithIndex; (j, weight) <- links) yield {
      val sizes = (size(i), size(j))
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

  /**
   * Lazy computation of the edge color to screen
   */
  lazy val renderEdgeColor = {
    val darkerColor = new Color(0.0, 0.0, 0.23)
    var tmpColor = List.empty[Color]
    val aextremums = (minAEdgeWeight, maxAEdgeWeight)
    val bextremums = (minBEdgeWeight, maxBEdgeWeight)
    def colorate(category: String) = (category match {
      case "Document" => colorScheme.primary.standard
      case "NGram" => colorScheme.tertiary.standard
      case other => colorScheme.secondary.standard
    })
    def getExtremum(category: String) = (category match {
      case "Document" => aextremums
      case "NGram" => bextremums
      case any => aextremums
    })

    val target = (0.4, 1.0)
    links.zipWithIndex map {
      case (mapIntDouble, from) =>
        val modeFrom = if (selected(from)) 'selected else if (highlighted(from)) 'highlighted else if (selectionValid) 'unselected else 'default
        val catFrom = category(from)
        val extr = getExtremum(catFrom)
        mapIntDouble foreach {
          case (to, weight) =>
            val catTo = category(to)
            val modeTo = if (selected(to)) 'selected else if (highlighted(to)) 'highlighted else if (selectionValid) 'unselected else 'default

            tmpColor ::= ((modeFrom, modeTo) match {
              case ('selected, any) => darkerColor.alpha(Maths.map(weight, extr, (0.86, 0.98)))
              // case (any, 'selected) => darkerColor.alpha(Maths.map(weight, extr, (0.86, 0.98)))
              case ('highlighted, any) => darkerColor.alpha(Maths.map(weight, extr, (0.60, 0.95)))
              //case (any, 'highlighted) => darkerColor.alpha(Maths.map(weight, extr, (0.60, 0.95)))
              case (any1, any2) =>
                if (selectionValid) {
                  // unselected
                  val t = colorate(catFrom)
                  t.blend(colorate(catTo)).saturateBy(0.78).alpha(Maths.map(weight, extr, (0.75, 0.94)))
                } else {
                  val t = colorate(catFrom)
                  t.blend(colorate(catTo)).alpha(Maths.map(weight, extr, (0.60, 0.90)))
                }
            })

        }
    }
    tmpColor.toArray
  }

  /**
   * Lazy computation of the edge position to screen
   */
  lazy val renderEdgePosition = {
    var t = List.empty[((Double, Double), (Double, Double))]
    links.zipWithIndex foreach {
      case (links, i) =>
        links.zipWithIndex foreach {
          case ((j, weight), _j) => t ::= (position(i), position(j))
        }
    }
    t.toArray
  }
  /**
   * Lazy computation of the edge position to screen
   */
  lazy val renderEdgeIndex = {
    var t = List.empty[(Int, Int)]
    links.zipWithIndex foreach {
      case (links, i) =>
        links.zipWithIndex foreach {
          case ((j, weight), _j) => t ::= (i, j)
        }
    }
    t.toArray
  }
  /**
   * Lazy computation of the edge weights
   */
  lazy val renderEdgeWeight = {
    var t = List.empty[Double]
    links.zipWithIndex foreach {
      case (links, i) =>
        links.zipWithIndex foreach {
          case ((j, weight), _j) => t ::= weight
        }
    }
    t.toArray
  }

  lazy val renderNodeShape = category.map {
    case "Document" => 'Square
    case "NGram" => 'Disk
    case any => 'Square
  }

  /**
   * Warm-up the lazy vals
   * Should not be called to often, since this is a costly operation!
   */
  def toGraph = {
    position
    renderNodeColor
    renderNodeBorderColor
    renderEdgeSize
    renderEdgeColor
    renderEdgePosition
    renderEdgeIndex
    renderEdgeWeight
    renderNodeShape


    // a list of positions + ID


    selectionNeighbourhood
    selectionNeighbourhoodCenter
    singlesCenter
    notSinglesCenter
    outDegreeExtremums
    inDegreeExtremums
    extremums
    extremumsSelection
    extremumsSelectionNeighbourhood
    nodeWeightExtremums
    edgeWeightExtremums

    this
  }

  // hashcode will change if nodes/links are added/deleted
  lazy val hashed = (uuid.toList.mkString("") + size.toList.mkString("") + links.map {
    case mapID => mapID.hashCode
  }.toList.mkString("")).hashCode

  lazy val debugStats = {
    "**DEBUG**\nlinks.size: " + links.size + "\nposition.size: " + position.size + "\ncolor.size: " + color.size + "\nuuid.size: " + uuid.size + "\ncategory.size: " + category.size + "\nselected.size: " + selected.size + "\nselection.size: " + selection.size + "\n**END DEBUG**"
  }

  /**
   * Check if a graph has any link between i and i (directed or undirected)
   */
  def hasAnyLink(i: Int, j: Int) = hasThisLink(i, j) | hasThisLink(j, i)

  /**
   * Check if a graph has a directed link (from i to j)
   */
  def hasThisLink(i: Int, j: Int) = if (links.size > i) links(i).contains(j) else false

  def isSingle(i: Int) = (inDegree(i) == 0 && outDegree(i) == 0)

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
          case v: Symbol => List[Symbol](v).toArray
          case v: (Double, Double) => List[(Double, Double)](v).toArray
          case v: Array[String] => List[Array[String]](v).toArray
          case v: Array[Symbol] => List[Array[Symbol]](v).toArray
          case v: Array[Double] => List[Array[Double]](v).toArray
          case v: Array[Int] => List[Array[Int]](v).toArray
          case v: List[Double] => List[List[Double]](v).toArray
          case v: List[String] => List[List[String]](v).toArray
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
          case v: Symbol =>
            var m = getArray[Symbol](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Symbol](v)).toArray
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
          case v: List[String] =>
            var m = getArray[List[String]](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[List[String]](v)).toArray
            m
          case v: Array[String] =>
            var m = getArray[Array[String]](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Array[String]](v)).toArray
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
            throw new Exception("FATAL ERROR, GOT ANY FOR " + v)
        }
      }
    }
    new Graph(newElements)
  }


  /**
   * List of selected nodes' IDs
   */
  lazy val selection: List[Int] = selected.zipWithIndex.filter {
    case (selected, i) => selected
  }.map {
    case (s, i) => i
  }.toList

  /**
   * List of selected nodes' attributes
   */
  lazy val selectionAttributes = {
    //println("mapping selection attributes: "+selection)
    selection.map {
      case i => lessAttributes(i)
    }.toList
  }


  /**
   * Return the current selection as a list of UUID:String
   */
  lazy val selectionUUID = selection.map {
    case i => getUuid(i)
  }.toList

  // { UUID : {neighbours}, UUID2; {neighbours}, ... }
  lazy val selectionNeighbours = {
    Map(selectionUUID.zipWithIndex: _*).map {
      case (uuid, i) => (uuid, neighbours(i))
    }
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
      "links" -> (if (links.size > i) links(i) else Map.empty[Int, Double]),
      "position" -> (if (position.size > i) position(i) else (0.0, 0.0)),
      "color" -> (if (color.size > i) color(i) else new Color(0.0, 0.0, 0.0)),
      "weight" -> (if (weight.size > i) weight(i) else 1.0),
      "size" -> (if (size.size > i) size(i) else 1.0),
      "category" -> (if (category.size > i) category(i) else ""),
      "content" -> (if (content.size > i) content(i) else ""), //Base64.encode(content(i)),
      "selected" -> (if (selected.size > i) selected(i) else false),
      "label" -> (if (label.size > i) label(i) else ""), // Base64.encode(label(i)),
      "rate" -> (if (rate.size > i) rate(i) else 0),
      "id" -> (if (uuid.size > i) uuid(i) else 0),
      "inDegree" -> (if (inDegree.size > i) inDegree(i) else 0),
      "outDegree" -> (if (outDegree.size > i) outDegree(i) else 0),
      "density" -> (if (density.size > i) density(i) else 0)
    )
  }

  /**
   * Return neighbours of a node ID - todo refactor to make it more efficient: lazy val for array of IDs, and another function for the minimal attributes
   *
   */
  /*
   lazy val neighboursIDs(i: Int): List[Int] = {
      ids.map {
        case id =>
        // println("  - mapping neighbours of node "+i+"..")
        uuid.zipWithIndex.filter{
            case (uj, ij) => hasAnyLink(ij,i) && (ij != i)
          } map { case (uuid,id) => id }
      }
   }
  }*/

  /**
   * Return neighbours of a node ID - todo refactor to make it more efficient: lazy val for array of IDs, and another function for the minimal attributes
   *
   */
   def neighbours(i: Int): Map[String, Map[String, Any]] = {
    (if (links.size > i) {
      // println("  - mapping neighbours of node "+i+"..")
      Map.empty[String,Map[String,Any]] ++ Map((uuid.zipWithIndex.filter{
          case (uj, ij) => hasAnyLink(ij,i) && (ij != i)
        } collect {

        case (uj,ij) =>
         (uj,minimalAttributes(ij))
      }): _*)
    }
    else
      Map.empty[String, Map[String, Any]])
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
      "weight" -> (if (weight.size > i) weight(i) else 0),
      //"size" -> size(i),
      "category" -> (if (category.size > i) category(i) else ""),
      "content" -> (if (content.size > i) content(i) else ""), //Base64.encode(content(i)),
      "selected" -> (if (selected.size > i) selected(i) else false),
      "label" -> (if (label.size > i) label(i) else ""), // Base64.encode(label(i)),
      "rate" -> (if (rate.size > i) rate(i) else 0),
      "id" -> (if (uuid.size > i) uuid(i) else 0),
      //"partition" -> (if (partition.size > i) partition(i) else 0),
      "degree" -> ((if (inDegree.size > i) inDegree(i) else 0) + (if (outDegree.size > i) outDegree(i) else 0))
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
      "weight" -> (if (weight.size > i) weight(i) else 0),
      //"size" -> size(i),
      "category" -> (if (category.size > i) category(i) else ""),
      //"content" -> (if (content.size > i) content(i) else ""),//Base64.encode(content(i)),
      // "selected" -> (if (selected.size > i) selected(i) else false),
      "label" -> (if (label.size > i) label(i) else ""), // Base64.encode(label(i)),
      //"rate" -> (if (rate.size > i) rate(i) else 0),
      "id" -> (if (uuid.size > i) uuid(i) else 0),
      //"partition" -> (if (partition.size > i) partition(i) else 0),
      "degree" -> ((if (inDegree.size > i) inDegree(i) else 0) + (if (outDegree.size > i) outDegree(i) else 0))

      //"density" -> density(i)
    )
  }

  /**
   * Get the map of all nodes
   */
  def allNodes: Map[String, Map[String, Any]] = {
    var nodeData = Map.empty[String, Map[String, Any]]
    for (i <- ids) nodeData += getUuid(i) -> lessAttributes(i)
    nodeData
  }

  def clearSelection = {
    this + ("selected" -> this.selected.map(c => false))
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


  def map[T](id: Int, column: String, filter: T => T): Graph = {
    set(id, column, filter(getArray[T](column)(id)))
  }


  def map[T](column: String, filter: T => T): Graph = {
    this + (column -> (getArray[T](column).map {
      f => filter(f)
    }))
  }

  def filterNodeVisible[T](column: String, filter: T => Boolean) = {
    this + ("visible" -> getArray[T](column).map {
      x => filter(x)
    })
  }

  def _filterNodeVisible[T](column: String, filter: T => Boolean) = {
    this + ("visible" -> getArray[T](column).map {
      x => filter(x)
    })
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
      case (key: String, entries: Array[Symbol]) =>
        (key, entries.zipWithIndex.filter {
          case (e, i) => conv(i) >= 0
        }.map(_._1).toArray)
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
      case (key: String, entries: Array[String]) =>
        (key, entries.zipWithIndex.filter {
          case (e, i) => conv(i) >= 0
        }.map(_._1).toArray)
      case (key: String, entries: Array[(Double, Double)]) =>
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


  /**
   * TODO refactor to use a generic field update function
   */
  def updatePosition(g: Graph): Graph = {

    val tmp1: Array[(Double, Double)] = position.zipWithIndex.map {
      case (elem, i) =>
        val id = g.id(uuid(i))
        if (id == -1) elem else g.position(id)
    }.toArray

    val tmp2: Array[Boolean] = selected.zipWithIndex.map {
      case (s, i) =>
        val id = g.id(uuid(i))
        if (id == -1) s else g.selected(id)
    }.toArray

    Graph.make(elements ++ Map[String, Any](
      "position" -> tmp1,
      "selected" -> tmp2) // need to recompute things
    )
  }

  def normalizePositions: Graph = this + ("visible" -> position.map {
    case (x, y) => (x - baryCenter._1, y - baryCenter._2)
  })


  /**
   * TODO refactor to use a generic field update function
   */
  def updatePositionWithCategory(g: Graph): Graph = {

    val tmp1: Array[(Double, Double)] = position.zipWithIndex.map {
      case (elem, i) =>
        val id = g.id(uuid(i))
        if (id == -1) {
          elem
        } else if (g.category(id).equalsIgnoreCase(category(i))) {
          g.position(id)
        } else {
          elem
        }
    }.toArray

    Graph.make(elements ++ Map[String, Any](
      "position" -> tmp1) // need to recompute things
    )
  }

  /**
   * TODO refactor to use a generic field update function
   */
  def updateSizeWithCategory(g: Graph): Graph = {

    val tmp1: Array[Double] = size.zipWithIndex.map {
      case (elem, i) =>
        val id = g.id(uuid(i))
        if (id == -1) {
          elem
        } else if (g.category(id).equalsIgnoreCase(category(i))) {
          g.size(id)
        } else {
          elem
        }
    }.toArray

    Graph.make(elements ++ Map[String, Any](
      "size" -> tmp1) // need to recompute things
    )
  }

  /**
   * TODO refactor to use a generic field update function
   */
  def updateLinksWithCategory(g: Graph): Graph = {

    val tmp1: Array[Map[Int, Double]] = links.zipWithIndex.map {
      case (elem, i) =>
        val id = g.id(uuid(i))
        if (id == -1) {
          elem
        } else if (g.category(id).equalsIgnoreCase(category(i))) {
          g.links(id)
        } else {
          elem
        }
    }.toArray

    Graph.make(elements ++ Map[String, Any](
      "links" -> tmp1) // need to recompute things
    )
  }

  /**
   * TODO refactor to use a generic field update function
   */
  def updateSelectedWithCategory(g: Graph): Graph = {

    val tmp2: Array[Boolean] = selected.zipWithIndex.map {
      case (s, i) =>
        val id = g.id(uuid(i))
        if (id == -1) {
          s
        } else if (g.category(id).equalsIgnoreCase(category(i))) {
          g.selected(id)
        } else {
          s
        }
    }.toArray

    Graph.make(elements ++ Map[String, Any](
      "selected" -> tmp2) // need to recompute things
    )
  }

}
