/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import eu.tinasoft._

import tinaviz.util.Color
import tinaviz.util.Color._

class Node (
  val uuid : String,
  val label : String = "Node",
  val position : (Double,Double) = (0,0),
  val velocity : (Double,Double) = (0,0),
  val color : Color =  new Color(0,0,0),
  //val size : (Double) = 1,
  //val category : String = "NGram",
  val attributes : Map[String,Any] =  Map.empty[String,Any],
  val links : List[(Int,Double)] = List.empty[(Int,Double)],
  val inDegree : Int = 0,
  val outDegree : Int = 0
) {

  override val toString = "<Node "+uuid+":"+label+":"+position._1+","+position._2+">"

  def hasLink(id:Int) : Boolean = { 
    links.foreach { 
      case (i,w) => 
        if (i==id) return true 
    }
    return false
  }
  
}
