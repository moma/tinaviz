/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import eu.tinasoft._
import eu.tinasoft.tinaviz.util.Color

object MutableGraph {
  
  def get[T](elements:Map[String,Any], key:String) : T = elements.get(key).get.asInstanceOf[T]
  
  /**
   * Should be an optimized factory
   */
  def makeDiff(newElements:Map[String,Array[Any]],
               oldElements:Map[String,Array[Any]]) = {

  }

  /**
   * Default, dumb factory
   */
  def make(elements:Map[String,Any]) = {
    elements.foreach{case (key,value) => println(" Entry: "+key+" ("+value+")")}
    // TODO: put nbNodes, nbEdges etc.. directly inside elements
    var g = new Graph(elements)
    g = g.computeAll
    //g = g.computeNbNodes
    // g = g.computeNbEdges
    // g = g.computeNbSingles
    //g = g.computeOutDegree
    //g = g.compute
    g
  }

  implicit def toGraph(mg:MutableGraph) : Graph = new Graph(mg.elements)
  
}

class MutableGraph (val _elements : Map[String,Any] = Map[String,Any]()) {
  
  var elements = _elements
  /**
   * Used for export to GEXF
   */
  def id (uuid:String) : Int = uuid.indexOf(uuid)

  def getUuuid (i:Int) = uuid(i)

  def get[T](key:String) : T = elements(key).asInstanceOf[T]
  def getArray[T](key:String) : Array[T] = get[Array[T]](key)
 
  // some built-in functions
  def linkIdArray = getArray[Array[Int]]("linkIdArray")
  def linkIdSet = getArray[Set[Int]]("linkIdSet")
  def linkWeightArray = getArray[Array[Double]]("linkWeightArray")
  def position = getArray[(Double,Double)]("position")
  def color = getArray[Color]("color")
  def weight = getArray[Double]("weight")
  def category = getArray[String]("category")
  def selected = getArray[Boolean]("selected")
  def label = getArray[String]("label")
  def rate = getArray[Int]("rate")
  def uuid = getArray[String]("uuid")

  def hasAnyLink(i:Int,j:Int) = hasThisLink(i,j) | hasThisLink(j,i)
  def hasThisLink(i:Int,j:Int) = linkIdSet(i).contains(j)

  def += (ikv:(String,Any)) : this.type = {
    elements += ikv
    this
  }
  def += (ikv:(Int,String,Any)) : this.type = {
    set(ikv._1,(ikv._2,ikv._3))
  }
  def set(id:Int,kv:(String,Any)) : this.type = {
    val k = kv._1
    //println("id: "+id+" kv: "+kv)
    elements += k -> {
      if (!elements.contains(k)) {
        kv._2 match {
          case v:Boolean => List[Boolean](v).toArray
          case v:Int => List[Int](v).toArray
          case v:Double => List[Double](v).toArray
          case v:Float => List[Float](v).toArray
          case v:String => List[String](v).toArray
          case v:Color => List[Color](v).toArray
          case v:(Double,Double) => List[(Double,Double)](v).toArray
          case v:Array[Double] => List[Array[Double]](v).toArray
          case v:Array[Int] => List[Array[Int]](v).toArray
          case v:List[Double] => List[List[Double]](v).toArray
          case v:List[Int] => List[List[Int]](v).toArray
          case v:Set[Int] => List[Set[Int]](v).toArray
          case v =>
            throw new Exception("UNRECOGNIZED TYPE")
            // List(v).toArray
        }
      } else {
        //println("key "+k+" already match!")
        val t = elements(k)
        //println("elements gave "+t+" ")

        kv._2 match {
          case v:Boolean =>
            var m = getArray[Boolean](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Boolean](v)).toArray
            m
          case v:Int =>
            var m = getArray[Int](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Int](v)).toArray
            m
          case v:Double =>
            var m = getArray[Double](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Double](v)).toArray
            m
          case v:Float =>
            var m = getArray[Float](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Float](v)).toArray
            m
          case v:String =>
            var m = getArray[String](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[String](v)).toArray
            m
          case v:Color =>
            var m = getArray[Color](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Color](v)).toArray
            m
          case v:(Double,Double) =>
            var m = getArray[(Double,Double)](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[(Double,Double)](v)).toArray
            m
          case v:List[Double] =>
            var m = getArray[List[Double]](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[List[Double]](v)).toArray
            m
          case v:Array[Double] =>
            var m = getArray[Array[Double]](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Array[Double]](v)).toArray
            m
          case v:List[Int] =>
            var m = getArray[List[Int]](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[List[Int]](v)).toArray
            m
          case v:Array[Int] =>
            var m = getArray[Array[Int]](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Array[Int]](v)).toArray
            m
          case v:Set[Int] =>
            var m = getArray[Set[Int]](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List[Set[Int]](v)).toArray
            m

          case v:Any =>
            // Actually, this is the only case called
            throw new Exception("FATAL FATAL FATAL, got any: "+v)
        }
      }
    }
    this
  }

  def set(kv:(String,Any)) : this.type = {
    elements += kv
    this
  }

  def computeAll : this.type = {
    computeNbNodes
    computeNbEdges
    computeNbSingles
    computeOutDegree
    computeInDegree
    computeExtremums
    computeBaryCenter
    this
  }

  def toGraph : Graph = {
    new Graph(elements)
  }
  
  def computeNbSingles : this.type = {
    var s = 0 ; linkIdArray.foreach{ case links => if (links.size ==0) s += 1 }
    elements += "nbSingles" -> s
    this
  }

  def computeNbEdges : this.type = {
    var s = 0 ; linkIdArray.foreach{ case links => s+= links.size }
    elements += "nbEdges" -> s
    this
  }
  def computeNbNodes : this.type = {
    elements += "nbNodes" -> uuid.size
    this
  }

  /*
   def computeNodeDegree (elements:Map[String,Any],i:Int) : Int = {

   val links = elements("linkIdSet").asInstanceOf[Array[Set[Int]]]
   var d = 0
   links.foreach { case m => if (m.contains(i)) d+= 1 }
   d
   }*/

  def computeOutDegree : this.type = {
    if (linkIdSet.size == 0) {
      elements += "minOutDegree" -> 0
      elements += "maxOutDegree" -> 0
      return this
    }
    var max = Int.MinValue
    var min = Int.MaxValue
    linkIdSet.foreach {
      case n =>
        val d = n.size
        if (d < min) min = d
        if (d > max) max = d
    }

    elements += "minOutDegree" -> min
    elements += "maxOutDegree" -> max
    this
  }
  def computeInDegree : this.type = {
    if (linkIdSet.size == 0) {
      elements += "minOutDegree" -> 0
      elements += "maxOutDegree" -> 0
      this
    }
    var max = Int.MinValue
    var min = Int.MaxValue
    var i = -1
    linkIdSet.foreach {
      case n =>
        i += 1
        var d = 0
        linkIdSet.foreach {
          case m=> if (m.contains(i)) d+= 1
        }
        if (d < min) min = d
        if (d > max) max = d
    }

    elements += "minOutDegree" -> min
    elements += "maxOutDegree" -> max
    this
  }

  def computeExtremums : this.type = {
    if (position.size == 0) {
      elements += "xMax" -> 0.0
      elements += "xMin" -> 0.0
      elements += "yMax" -> 0.0
      elements += "yMin" -> 0.0
      return this
    }
    var xMax = Double.MinValue
    var xMin = Double.MaxValue
    var yMax = Double.MinValue
    var yMin = Double.MaxValue
    var i = -1
    position.foreach {
      case (x,y) =>
        if (x < xMin) xMin = x
        if (x > xMax) xMax = x
        if (y < yMin) yMin = y
        if (y > yMax) yMax = y
    }
    elements += "xMax" -> xMax
    elements += "xMin" -> xMin
    elements += "yMax" -> yMax
    elements += "yMin" -> yMin
    this
  }
  def computeBaryCenter : this.type = {
    var p = (0.0,0.0)
    var N = position.size.toDouble
    position.foreach { case (x,y) =>  p = (p._1+x, p._2+y) }
    elements += "baryCenter" -> (if (N != 0) (p._1/N,p._2/N) else (0.0,0.0))
    this
  }

}
