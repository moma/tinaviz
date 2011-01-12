/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import eu.tinasoft._
import eu.tinasoft.tinaviz.util.Color
import tinaviz.util.Vector

object Graph {
  
  def get[T](elements:Map[String,Array[Any]], key:String) : T = elements.get(key).get.asInstanceOf[T]
  
  /**
   * Should be an optimized factory
   */
  def makeDiff(newElements:Map[String,Array[Any]],
               oldElements:Map[String,Array[Any]]) = {

  }

   /**
    * Default, dumb factory
    */
  def make(elements:Map[String,Array[Any]]) = {
    new Graph (elements,
               nbNodes(elements),
               nbEdges(elements),
               nbSingles(elements),
               outDegree(elements),
               inDegree(elements),
               extremums(elements),
               baryCenter(elements))
  }
  

  def nbSingles(elements:Map[String,Array[Any]]) = {
    val links = elements("links").asInstanceOf[Array[List[Int]]]
    var s = 0
    links.foreach {  case n => if (n.size ==0) s += 1 }
    s 
  }
  
  def nbNodes(elements:Map[String,Array[Any]]) = elements("uuid").size
  def nbEdges(elements:Map[String,Array[Any]]) = {
    val links = elements("links").asInstanceOf[Array[Set[Int]]]
    var s = 0;
    links.foreach {  case n => s+=n.size }
    s 
  }


  def computeNodeDegree (elements:Map[String,Array[Any]],i:Int) : Int = {

    val links = elements("links").asInstanceOf[Array[Set[Int]]]
    var d = 0
    links.foreach { case m => if (m.contains(i)) d+= 1 }
    d
  }
  
  def outDegree(elements:Map[String,Array[Any]]) : (Int,Int) = {
    val links = elements("links").asInstanceOf[Array[Set[Int]]]
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
  def inDegree(elements:Map[String,Array[Any]]) : (Int,Int) = {
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
  
  def extremums(elements:Map[String,Array[Any]]) = Vector.extremums(
    elements("position").asInstanceOf[Array[(Double,Double)]]
  )

  def baryCenter(elements:Map[String,Array[Any]]) : (Double,Double) = {
    val nodes = elements("position").asInstanceOf[Array[(Double,Double)]]
    var p = (0.0,0.0)
    var N = nodes.size.toDouble
    nodes.foreach { case n =>  p = (p._1+n._1, p._2+n._2) }
    if (N != 0) (p._1/N,p._2/N) else (0.0,0.0)
  }
  
}

class Graph (val elements : Map[String,Array[Any]] = Map[String,Array[Any]](),
             val nbNodes : Int = 0,
             val nbEdges : Int = 0,
             val nbSingles : Int = 0,
             val outDegree : (Int,Int) = (0,0),
             val inDegree : (Int,Int) = (0,0),
             val extremums : ((Double,Double),(Double,Double)) = ((.0,.0),(.0,.0)),
             val baryCenter : (Double, Double) = (0.0,0.0)
) {
  
  
  /**
   * Used for export to GEXF
   */
  def id (uuid:String) : Int = uuid.indexOf(uuid)

  def getUuuid (i:Int) = uuid(i)

  def get[T](key:String) : T = elements.get(key).asInstanceOf[T]
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
}
