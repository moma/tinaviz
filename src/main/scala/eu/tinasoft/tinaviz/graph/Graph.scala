/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

class Graph (val nodes : Set[Node] = Set[Node](),
             val properties : Map[String,Any] = Map[String,Any]()) {

  /**
   * Used for export to GEXF
   */
  def getId(node:Node) : Int = {
    var i = 0
    nodes.foreach{
      case node =>
        if (node==node) return i
        i += 1
    }
    0
  }
}
