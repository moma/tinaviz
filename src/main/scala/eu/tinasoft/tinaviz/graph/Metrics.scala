package eu.tinasoft.tinaviz.graph

/**
 * Created by IntelliJ IDEA.
 * User: jbilcke
 * Date: 2/17/11
 * Time: 12:33 PM
 */
object Metrics  {

  /**
   * compute the amount of information added to thiq by g
   */
  def activity(g:Graph,f: Graph): Double = {
    def max(x: Double, y: Double): Double = if (x < y) y else x
    var addNodes = 0.0
    var deletedNodes = 0.0
    var addEdges = 0.0
    var deletedEdges = 0.0
    f.uuid.zipWithIndex foreach {
      case (u, i) => if (!g.has(u)) addNodes += 1.0
    }
    g.uuid.zipWithIndex foreach {
      case (u, i) => if (!f.has(u)) deletedNodes += 1.0
    }
    val activity1 = g.activity * g.entropy
    val count = g.nbNodes + f.nbNodes
    val activity2 = if (count > 0) ((addNodes + deletedNodes) / count) else 0
    //val activity2 = if (count > 0) Maths.map(((addNodes + deletedNodes) / count),(0.0,1.0),(0.1,0.99)) else 0
    val a = max(activity1, activity2)
    //println("activity: " + a)
    a
  }

  /**
   * Compute the number of single nodes
   */
  def nbSingles(g:Graph) : Int = {
    var s = 0
    g.links.foreach { case lnk => if (lnk.size == 0) s += 1 }
    s
  }

  /**
   * Compute the number of edges
   */
  def nbEdges(g:Graph) : Int = {
    var s = 0
    g.links.foreach { case lnk => s += lnk.size }
    s
  }

  /**
   * Compute the number of nodes
   */
  def nbNodes(g:Graph) : Int = g.uuid.size

  /**
   * Compute the out degree array
   */
  def outDegree(g:Graph) : Array[Int] = {
    val _outDegree = g.links.map { case m => m.size }
    _outDegree.toArray
  }

  /**
   * Compute the in degree array
   */
  def inDegree(g:Graph) : Array[Int] = {

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
   * Compute the out degree extremums
   */
  def outDegreeExtremums(g:Graph) : (Int,Int) = {
    if (g.links.size == 0) {
      (0,0)
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
  def inDegreeExtremums(g:Graph) : (Int,Int) = {
    if (g.links.size == 0) {
      (0,0)
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
      (min,max)
    }
  }

  /**
   * Compute the extremums (X min, X max, Y min, Y max)
   */
  def extremums(g:Graph) : (Double,Double,Double,Double) = {
    if (g.position.size == 0) {
    (0.0,0.0,0.0,0.0)
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
     (xMax,xMin, yMax, yMin)
    }
  }

  /**
   * Compute the node weight extremums (min node weight, max node weight)
   * return a Tuple of Double
   */
  def nodeWeightExtremums (g:Graph) : (Double,Double) = {
    if (g.position.size == 0) {
       (0.0,0.0)
    } else {
    var max = Double.MinValue
    var min = Double.MaxValue
    g.weight foreach {
      case x =>
        if (x < min) min = x
        if (x > max) max = x
    }
    (min, max)
    }
  }

  /**
   * Compute the edge weight extremums (min edge weight, max edge weight)
   * return a Tuple of Double
   */
  def edgeWeightExtremums (g:Graph) : (Double,Double) = {
    if (g.links.size == 0)  {
      (0.0,0.0)
    } else {
    var max = Double.MinValue
    var min = Double.MaxValue
    g.links foreach {
      case lnks =>
        lnks.foreach {
          case (id, weight) =>
            if (weight < min) min = weight
            if (weight > max) max = weight
        }
    }
    (min, max)
    }
  }

  /**
   * Compute a graph's barycenter
   */
  def baryCenter (g:Graph) : (Double,Double) = {
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
  def selectionCenter (g:Graph) : (Double,Double) = {
    var p = (0.0, 0.0)
    val N = g.position.size.toDouble
    g.position.zipWithIndex foreach {
      case ((x, y),i) =>
        if (g.selected(i)) p = (p._1 + x, p._2 + y)
    }
    if (N != 0) (p._1 / N, p._2 / N) else (0.0, 0.0)
  }
}