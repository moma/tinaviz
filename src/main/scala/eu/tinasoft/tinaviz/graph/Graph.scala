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
    g = g.computeNbSingles
    g = g.computeNbEdges
    g
  }
  

  def nbNodes(elements:Map[String,Any]) = elements("uuid").asInstanceOf[Array[String]].size


  def computeNodeDegree (elements:Map[String,Any],i:Int) : Int = {

    val links = elements("linkIdSet").asInstanceOf[Array[Set[Int]]]
    var d = 0
    links.foreach { case m => if (m.contains(i)) d+= 1 }
    d
  }
  
  def outDegree(elements:Map[String,Any]) : (Int,Int) = {
    val links = elements("linkIdSet").asInstanceOf[Array[Set[Int]]]
    if (links.size == 0) return (0,0)
    var max = Int.MinValue 
    var min = Int.MaxValue
    links.foreach { 
      case n =>
        val d = n.size
        if (d < min) min = d
        if (d < max) max = d
    }
    (min,max)
  }
  def inDegree(elements:Map[String,Any]) : (Int,Int) = {
    val links = elements("links").asInstanceOf[Array[Set[Int]]]
    if (links.size == 0) return (0,0)
    var max = Int.MinValue 
    var min = Int.MaxValue
    var i = 0
    links.foreach { 
      case n =>
        val d = computeNodeDegree(elements, i)
        if (d < min) min = d
        if (d < max) max = d
        i += 1
    }
    (min,max)
  }
  
  def extremums(elements:Map[String,Any]) = Vector.extremums(
    elements("position").asInstanceOf[Array[(Double,Double)]]
  )

  def baryCenter(elements:Map[String,Any]) : (Double,Double) = {
    val nodes = elements("position").asInstanceOf[Array[(Double,Double)]]
    var p = (0.0,0.0)
    var N = nodes.size.toDouble
    nodes.foreach { case n =>  p = (p._1+n._1, p._2+n._2) }
    if (N != 0) (p._1/N,p._2/N) else (0.0,0.0)
  }
  
}

class Graph (val elements : Map[String,Any] = Map[String,Any]()) {
  
  
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


  def set(id:Int,kv:(String,Any)) {
    val k = kv._1

    var newElements = elements
    newElements += k -> {
      if (!elements.contains(k)) {
        kv._2 match {
          case v:Boolean => List[Boolean](v).toArray
          case v:Int => List[Int](v).toArray
          case v:Double => List[Double](v).toArray
          case v:Float => List[Float](v).toArray
          case v:String => List[String](v).toArray
          case v => List(v).toArray
        }
      }
      else {
        kv._2 match {
          case v:Boolean =>
            var m = getArray[Boolean](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List(v)).toArray
            m
          case v:Int =>
            var m = getArray[Int](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List(v)).toArray
            m
          case v:Double =>
            var m = getArray[Double](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List(v)).toArray
            m
          case v:Float =>
            var m = getArray[Float](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List(v)).toArray
            m
          case v:String =>
            var m = getArray[String](k)
            if (id < m.size) m(id) = v else m = (m.toList ::: List(v)).toArray
            m
          case x => throw new Exception("not implemented")
        }
      }
    }
    new Graph (newElements)
  }

  def set(kv:(String,Any)) = new Graph (elements + kv)
 

  def computeNbSingles = {
    var s = 0 ; linkIdArray.foreach{ case links => if (links.size ==0) s += 1 }
    new Graph (elements + ("nbSingles" -> s))
  }

  def computeNbEdges = {
    var s = 0 ; linkIdArray.foreach{ case links => s+= links.size }
    new Graph (elements + ("nbEdges" -> s))
  }
  def computeNbNodes = new Graph (elements + ("nbNodes" -> uuid.size))
 

}
