/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

class Node (
  val uuid : String,
  val label : String = "Node",
  val position : (Double,Double) = (0,0),
  val color : (Int,Int,Int) = (0,0,0),
  val size : (Double) = 1,
  val links : List[(Int,Double)] = List.empty[(Int,Double)]
) {


  override val toString = "<Node "+uuid+":"+label+":"+position._1+","+position._2+">"
  
  def toNode = { new Node ( uuid,label,position, color, size, links) }
}
