/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.sketch

import eu.tinasoft._

import tinaviz.util._
import tinaviz.util.Color._
import tinaviz.scene._
import tinaviz.graph._

object Sketch {
  implicit def graphToSketch(graph: Graph): Sketch = {
    val sketch = new Sketch
    sketch.update(graph)
    sketch
  }

  implicit def sketchToScene(sketch: Sketch): Scene = {
    new Scene(
      sketch.background,
      sketch.foreground,
      sketch.labelColor,

      sketch.nodePositionLayer.size,
      sketch.edgePositionLayer.size,

      // nodes
      sketch.nodePositionLayer,
      sketch.nodeColorLayer,
      sketch.nodeBorderColorLayer,
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

case class Sketch(
                   var colors: Scheme = Rio,
                   var background: Color = new Color(0.0, 0.0, 1.0),
                   var foreground: Color = new Color(0.0, 1.0, 0.0),
                   var labelColor: Color = new Color(0.0, 1.0, 0.0),
                   var selectionZoneColor: Color = new Color(0.0, 1.0, 0.0),

                   // nodes
                   var nodePositionLayer: Array[(Double, Double)] = Array.empty[(Double, Double)],
                   var nodeColorLayer: Array[Color] = Array.empty[Color],
                   var nodeBorderColorLayer: Array[Color] = Array.empty[Color],
                   var nodeShapeLayer: Array[Symbol] = Array.empty[Symbol],
                   var nodeSizeLayer: Array[Double] = Array.empty[Double],

                   var nodeLabelLayer: Array[String] = Array.empty[String],
                   // edges
                   var edgePositionLayer: Array[((Double, Double),
                     (Double, Double))] = Array.empty[((Double, Double),
                     (Double, Double))],
                   var edgeColorLayer: Array[Color] = Array.empty[Color],
                   var edgeWeightLayer: Array[Double] = Array.empty[Double]
                   ) {

  /**
   * Update all layers
   *
   * TODO: there is place for optimization here, by only update what has changed
   *
   */
  def update(graph: Graph) {
    reset

    updateNodeColors(graph)
    updateNodePositions(graph)
    updateNodeLabels(graph)
    updateNodeShapes(graph)
    updateNodeSizes(graph)
  }

  /*
   def applyPatch(graph:Graph,changes:Set[Symbol]) {
   // changed allow us to know if we already recompiled a field
   var changed = changes.map{ (_,false) }.toMap

   changes.foreach {
   case c =>
   if (!changed(c)) {
   changed += c -> true
   c match {
   case 'position => updateNodePositions(graph)
   case 'color => updateNodeColorss(graph)
   case err => throw new Exception("unknow symbol "+err)
   }
   }
   }
   }
   */
  def reset() {
    background = new Color(0.0, 0.0, 1.0)
    foreground = new Color(0.0, 1.0, 0.0)
    labelColor = new Color(0.0, 1.0, 0.0)

    // nodes
    nodePositionLayer = Array.empty[(Double, Double)]
    nodeColorLayer = Array.empty[Color]
    nodeBorderColorLayer = Array.empty[Color]
    nodeShapeLayer = Array.empty[Symbol]
    nodeSizeLayer = Array.empty[Double]

    nodeLabelLayer = Array.empty[String]
    // edges
    edgePositionLayer = Array.empty[((Double, Double), (Double, Double))]
    edgeColorLayer = Array.empty[Color]
    edgeWeightLayer = Array.empty[Double]
  }

  /**
   * Update the nodes' positions layer
   *
   * This update is transitive, and will updare edge positions as well
   *
   */
  def updateNodePositions(graph: Graph) {
    nodePositionLayer = graph.position
    updateEdgePositions(graph)
  }

  /**
   * Update the nodes' colors layer
   *
   * This update is transitive, and will updare edge colors as well
   *
   */
  def updateNodeColors(graph: Graph) {
    //println("selected length: "+graph.selected.size)
    val selectionValid = (graph.selection.size > 0)
    nodeColorLayer = graph.selected.zipWithIndex map {
      case (selected, i) =>
        val mode = if (selected) 'selected else if (graph.highlighted(i)) 'highlighted else if (selectionValid) 'unselected else 'default
        val color = graph category i match {
          case "Document" => colors.primary
          case "NGram" => colors.tertiary
          case other => colors.secondary
        }
        mode match {
          case 'selected => color.standard
          case 'highlighted => color.standard
          case 'unselected => color.lighter.saturation(0.3)
          case 'default => color.light
        }
    }

    nodeBorderColorLayer = graph.selected.zipWithIndex map {
      case (selected, i) =>
        val mode = if (selected) 'selected else if (graph.highlighted(i)) 'highlighted else if (selectionValid) 'unselected else 'default
        val color = graph category i match {
          case "Document" => colors.primary
          case "NGram" => colors.tertiary
          case other => colors.secondary
        }
        mode match {
          case 'selected => new Color(0.0, 0.0, 0.23)
          case 'highlighted => new Color(0.0, 0.0, 0.23)
          case 'unselected => color.darker.saturation(0.3)
          case 'default => color.darker
        }
    }
    updateEdgeColors(graph)
  }

  /**
   * Update the nodes' shapes layer
   *
   * By default, we take the node category and try to map it to
   */
  def updateNodeShapes(graph: Graph) {
    nodeShapeLayer = graph.category map {
      case "Document" => 'Square
      case "NGram" => 'Disk
      case any => 'Square
    }
    updateEdgeColors(graph)
  }

  /**
   * Update the nodes' size layer
   *
   * By default, we take the node weight and try to map it to
   */
  def updateNodeSizes(graph: Graph) {
    nodeSizeLayer = graph.size
    updateEdgeSizes(graph)
    updateEdgeColors(graph)
  }

  /**
   * Update the nodes' labels layer
   *
   *
   */
  def updateNodeLabels(graph: Graph) {
    nodeLabelLayer = graph.label
  }

  /**
   * Update the edges' positions layer
   *
   */
  private def updateEdgePositions(graph: Graph) {
    var source = -1

    //var tmpEdges = List.empty[((Double,Double),(Double,Double),Double,Color)]
    var tmpPosition = List.empty[((Double, Double), (Double, Double))]
    var tmpColor = List.empty[Color]
    var tmpWeight = List.empty[Double]

    //println("updateEdgePositions of "+graph)

    val tmpNodes = graph.links.zipWithIndex map {
      case (links, i) =>
        links.zipWithIndex foreach {
          case ((j, weight), _j) =>
            val src = graph position i
            val trg = graph position j
            //val srcd = graph density i
            val trgd = graph density j
            val color = nodeColorLayer(i).blend(nodeColorLayer(j))
            tmpPosition ::= (src, trg)
            tmpColor ::= color
            tmpWeight ::= weight
        }
    }

    edgePositionLayer = tmpPosition.toArray
    edgeWeightLayer = tmpWeight.toArray
    edgeColorLayer = tmpColor.toArray
  }


  /**
   * Update the edges' colors layer
   *
   */
  def updateEdgeColors(graph: Graph) {
    var tmpColor = List.empty[Color]
    //println(" node color size: "+nodeColorLayer.size)
    //println("graph links size: "+graph.links.size)
    graph.links.zipWithIndex map {
      case (mapIntDouble, from) =>
        mapIntDouble foreach {
          // todo: use weight to ponderate the color?
          case (to, weight) =>
          // FEATURE we want the edge color to be a mix of source node and target node color
          // FEATURE we want the edge color to be less saturated
            val a = nodeColorLayer(from)
            val b = nodeColorLayer(to)
            val c = a.blend(b)
            val d = c.saturateBy(0.4)
            tmpColor ::= d
        }
    }
    edgeColorLayer = tmpColor.toArray
  }

  /**
   * Update the edges' weight layer
   *
   */
  def updateEdgeSizes(graph: Graph) {

    //val min


    val min = graph.get[Double]("minEdgeWeight")
    val max = graph.get[Double]("maxEdgeWeight")


    edgeWeightLayer = (for ((links, i) <- graph.links.zipWithIndex; (j, weight) <- links) yield {
      val sizes = (graph.size(i), graph.size(j))
      val avgSize = (sizes._1 + sizes._2) / 2.0
      val w = Maths.limit(avgSize, Maths.min(sizes), Maths.max(sizes))
      // print("  w: "+w)
      //val r = weight * 1.0
      val r = 1.0 * 1.0
      //println("  r: "+r)
      r
    }).toArray
  }

}
