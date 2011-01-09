/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz


import eu.tinasoft.tinaviz.graph.Graph

object Sketch {
  implicit def graphToSketch (graph:Graph) : Sketch = {
    val sketch = new Sketch
    sketch.updateAll(graph)
    sketch
  }
  implicit def sketchToScene (sketch:Sketch) : Scene = {
    new Scene(
      sketch.background,
      sketch.foreground,
      sketch.labelColor,

      sketch.nodePositionLayer.size,
      sketch.edgePositionLayer.size,

      // nodes
      sketch.nodePositionLayer,
      sketch.nodeColorLayer,
      sketch.nodeShapeLayer,
      sketch.nodeSizeLayer,

      sketch.nodeLabelLayer,
      // edges
      sketch.edgePositionLayer,
      sketch.edgeColorLayer,
      sketch.edgeWeightLayer

    )

  }

}
case class Sketch (
  var background : Color =  new Color(255,255,255),
  var foreground : Color =  new Color(0,0,0),
  var labelColor : Color = new Color (0,0,0),

  // nodes
  var nodePositionLayer : Array[(Double,Double)] = Array.empty[(Double,Double)],
  var nodeColorLayer : Array[Color] = Array.empty[Color],
  var nodeShapeLayer : Array[Symbol] = Array.empty[Symbol],
  var nodeSizeLayer : Array[Double] = Array.empty[Double],

  var nodeLabelLayer : Array[String] = Array.empty[String],
  // edges
  var edgePositionLayer : Array[((Double,Double),
                                 (Double,Double))] = Array.empty[((Double,Double),
                                                                  (Double,Double))],
  var edgeColorLayer : Array[Color] = Array.empty[Color],
  var edgeWeightLayer : Array[Double] = Array.empty[Double]
)  {

  /**
   * Update all layers
   * 
   */
  def updateAll(graph:Graph) {
    updateNodePositions(graph)
    updateNodeColors(graph)
    updateNodeLabels(graph)
    updateNodeShapes(graph)
    updateNodeSizes(graph)
  }

  /**
   * Update the nodes' positions layer
   *
   * This update is transitive, and will updare edge positions as well
   *
   */
  def updateNodePositions(graph:Graph) {
    nodePositionLayer = graph.nodes.map {
      case n => n.position
    }.toArray
    updateEdgePositions(graph)
  }

  /**
   * Update the nodes' colors layer
   *
   * This update is transitive, and will updare edge colors as well
   *
   */
  def updateNodeColors(graph:Graph) {
    nodeColorLayer = graph.nodes.map {
      case n => n.color
    }.toArray
    updateEdgeColors(graph)
  }

  /**
   * Update the nodes' shapes layer
   *
   * By default, we take the node category and try to map it to
   */
  def updateNodeShapes(graph:Graph) {
    nodeShapeLayer = graph.nodes.map {
      case n =>
        try {
          n.attributes("category") match {
            case "Document" => 'Square
            case "NGram" => 'Disk
          }
        } catch {
          case x => 'Disk
        }
    }.toArray
    updateEdgeColors(graph)
  }

  /**
   * Update the nodes' size layer
   *
   * By default, we take the node weight and try to map it to
   */
  def updateNodeSizes(graph:Graph) {
    nodeSizeLayer = graph.nodes.map {
      case n =>
        try {
          n.attributes("weight") match {
            case f:Float => f.toDouble
            case i:Int => i.toDouble
            case d:Double => d
          }
        } catch {
          case x => 1.0
        }
    }.toArray
    updateEdgeColors(graph)
  }

  /**
   * Update the nodes' labels layer
   *
   *
   */
  def updateNodeLabels(graph:Graph) {
    nodeLabelLayer = graph.nodes.map {
      case n => n.label
    }.toArray
  }

  /**
   * Update the edges' positions layer
   * 
   */
  private def updateEdgePositions(graph:Graph) {
    var t = for (node <- graph.nodes; link <- node.links)
      yield (node.position,graph.node(link._1).position)
    edgePositionLayer = t.toArray
  }

  /**
   * Update the edges' colors layer
   * 
   */
  def updateEdgeColors(graph:Graph) {
    var t = for (node <- graph.nodes; link <- node.links)
      yield node.color.blend(graph.node(link._1).color)
    edgeColorLayer = t.toArray
  }
}
