/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import eu.tinasoft.tinaviz.Color

class Node (
  val uuid : String,
  val label : String = "Node",
  val position : (Double,Double) = (0,0),
  val color : Color =  new Color(0,0,0),
  //val size : (Double) = 1,
  //val category : String = "NGram",
  val attributes : Map[String,Any] =  Map.empty[String,Any],
  val links : List[(Int,Double)] = List.empty[(Int,Double)]
) {


  override val toString = "<Node "+uuid+":"+label+":"+position._1+","+position._2+">"
  
  def toNode = { new Node ( uuid,
                           label,
                           position,
                           color,
                           //size,
                           attributes,
                           links) }
}
