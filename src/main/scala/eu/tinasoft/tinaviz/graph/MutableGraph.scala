/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

class MutableGraph {

  var nodes : Set[Node] = Set.empty
  var properties : Map[String,Any] = Map.empty

  def addNode(node:Node) = {
    nodes += node
  }

  def setProperty(key:String,value:Any) = {
    properties += key -> value
  }

  /**
   * Return a new immutable graph
   */
  def toGraph : Graph = { new Graph(nodes, properties) }
}
