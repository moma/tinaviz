/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

object MutableNode {

}

case class MutableNode (
  var uuid : String,
  var label : String = "Node",
  var position : (Double,Double) = (0,0),
  //var color : (Int,Int,Int) = (0,0,0),
  //var size : (Double) = 1,
  //var category : String = "NGram",
  var attributes : Map[String,Any] =  Map.empty[String,Any],
  var links : List[(Int,Double)] = List.empty[(Int,Double)]
) {

  override val toString = "<Node "+uuid+":"+label+":"+position._1+","+position._2+">"

  def toNode = { new Node ( uuid,
                           label,
                           position,
                           //color,
                           //size,
                           attributes,
                           links) }
  
  def addNeighbour(id:Int,weight:Double) = links ::= (id,weight)
}
