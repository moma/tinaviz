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
 
  
}

class Graph (val elements : Map[String,Any] = Map[String,Any]()) {
  
  
  /**
   * Used for export to GEXF
   */
  def id (_uuid:String) : Int = uuid.indexOf(_uuid)

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
  def inDegree = getArray[Int]("inDegree")
  def outDegree = getArray[Int]("outDegree")

  def hasAnyLink(i:Int,j:Int) = hasThisLink(i,j) | hasThisLink(j,i)
  def hasThisLink(i:Int,j:Int) = linkIdSet(i).contains(j)

  def + (kv:(String,Any)) = {
   new Graph(elements + kv)
  }

  def + (id:Int,k:String,v:Any) = {
    set(id,k,v)
  }
  def set(id:Int,k:String,value:Any) = {
    //println("id: "+id+" kv: "+kv)
    var newElements = elements
    newElements += k -> {
      if (!elements.contains(k)) {
        value match {
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

        value match {
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
    new Graph (newElements)
  }

  def set(kv:(String,Any)) = new Graph (elements + kv)

  def computeAll = {
    var g = this
    g = g.computeNbNodes
    g = g.computeNbEdges
    g = g.computeNbSingles
    g = g.computeOutDegree
    g = g.computeInDegree
    g = g.computeExtremums
    g.computeBaryCenter
  }

  def computeNbSingles = {
    var s = 0 ; linkIdArray.foreach{ case links => if (links.size ==0) s += 1 }
    new Graph (elements + ("nbSingles" -> s))
  }

  def computeNbEdges = {
    var s = 0 ; linkIdArray.foreach{ case links => s+= links.size }
    new Graph (elements + ("nbEdges" -> s))
  }
  def computeNbNodes = new Graph (elements + ("nbNodes" -> uuid.size))

  /*
   def computeNodeDegree (elements:Map[String,Any],i:Int) : Int = {

   val links = elements("linkIdSet").asInstanceOf[Array[Set[Int]]]
   var d = 0
   links.foreach { case m => if (m.contains(i)) d+= 1 }
   d
   }*/

  def computeOutDegree : Graph = {
    val _outDegree = linkIdSet.map { case n => n.size }
    new Graph(elements ++ Map[String,Any]("outDegree" -> _outDegree))
  }

  def computeInDegree : Graph = {

    val _inDegree = uuid.zipWithIndex.map {
      case (n,i) =>
        var d = 0
        linkIdSet.foreach {
          case m=> if (m.contains(i)) d+= 1
        }
        d
    }
    new Graph(elements ++ Map[String,Any]("inDegree" -> _inDegree))
  }
  def computeOutDegreeExtremums : Graph = {
    if (linkIdSet.size == 0) 
      return new Graph(elements ++ Map[String,Any]("minOutDegree" -> 0,
                                                   "maxOutDegree" -> 0))
    var max = Int.MinValue
    var min = Int.MaxValue
    linkIdSet.foreach {
      case n =>
        val d = n.size
        if (d < min) min = d
        if (d > max) max = d
    }
    new Graph(elements ++ Map[String,Any]("minOutDegree" -> min,
                                          "maxOutDegree" -> max))
  }
  def computeInDegreeExtremums : Graph = {
    if (linkIdSet.size == 0)
      return new Graph(elements ++ Map[String,Any]("minOutDegree" -> 0,
                                                   "maxOutDegree" -> 0))
    var max = Int.MinValue
    var min = Int.MaxValue

    linkIdSet.zipWithIndex foreach {
      case (n,i) =>
        var d = 0
        linkIdSet.foreach {
          case m=> if (m.contains(i)) d+= 1
        }
        if (d < min) min = d
        if (d > max) max = d
    }
    new Graph(elements ++ Map[String,Any]("minOutDegree" -> min,
                                          "maxOutDegree" -> max))
  }

  def computeExtremums : Graph = {
    if (position.size == 0)
      return new Graph(elements ++ Map[String,Any]("xMax" -> 0.0,
                                                   "xMin" -> 0.0,
                                                   "yMax" -> 0.0,
                                                   "yMin" -> 0.0))
    var xMax = Double.MinValue
    var xMin = Double.MaxValue
    var yMax = Double.MinValue
    var yMin = Double.MaxValue
    position.foreach {
      case (x,y) =>
        if (x < xMin) xMin = x
        if (x > xMax) xMax = x
        if (y < yMin) yMin = y
        if (y > yMax) yMax = y
    }
    return new Graph(elements ++ Map[String,Any]("xMax" -> xMax,
                                                 "xMin" -> xMin,
                                                 "yMax" -> yMax,
                                                 "yMin" -> yMin))
  }
  def computeBaryCenter : Graph = {
    var p = (0.0,0.0)
    var N = position.size.toDouble
    position.foreach { case (x,y) =>  p = (p._1+x, p._2+y) }
    new Graph(elements ++ Map[String,Any]("baryCenter" -> (if (N != 0) (p._1/N,p._2/N) else (0.0,0.0))))
  }
  def map[T](id:Int,column:String,filter: T => T) : Graph = {
    set(id, column, filter(getArray[T](column)(id)))
  }
  /*
  def pfilter[T](id:Int,column:String)(filter: PartialFunction[T, T]) = {
  set(id, column, filter(getArray[T](column)(id)))
  }
*/
  def map[T](column:String,filter: T => T) : Graph = {
    new Graph(elements ++ Map[String,Any](
        column -> getArray[T](column).map{
          case f=>filter(f)
        }
    )
  }
}
