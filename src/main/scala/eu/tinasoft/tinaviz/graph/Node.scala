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
  val size : (Double) = 1
  ) {


  override val toString = "<Node "+uuid+":"+label+":"+position._1+","+position._2+">"
}
