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
  implicit def graphToSketch (graph:Graph) : Sketch = {
    val sketch = new Sketch
    sketch.overwrite(graph)
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
case class Sketch (
  var colors : Scheme = Rio,
  var background : Color =  new Color(0.0,0.0,1.0),
  var foreground : Color =  new Color(0.0,1.0,0.0),
  var labelColor : Color = new Color (0.0,1.0,0.0),
  var selectionZoneColor : Color = new Color (0.0,1.0,0.0),

  // nodes
  var nodePositionLayer : Array[(Double,Double)] = Array.empty[(Double,Double)],
  var nodeColorLayer : Array[Color] = Array.empty[Color],
  var nodeBorderColorLayer : Array[Color] = Array.empty[Color],
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
  def overwrite(graph:Graph) {
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
    background = new Color (0.0,0.0,1.0)
    foreground = new Color (0.0,1.0,0.0)
    labelColor = new Color (0.0,1.0,0.0)

    // nodes
    nodePositionLayer  = Array.empty[(Double,Double)]
    nodeColorLayer  = Array.empty[Color]
    nodeBorderColorLayer  = Array.empty[Color]
    nodeShapeLayer  = Array.empty[Symbol]
    nodeSizeLayer  = Array.empty[Double]

    nodeLabelLayer  = Array.empty[String]
    // edges
    edgePositionLayer  = Array.empty[((Double,Double),(Double,Double))]
    edgeColorLayer  = Array.empty[Color]
    edgeWeightLayer = Array.empty[Double]
  }

  /**
   * Update the nodes' positions layer
   *
   * This update is transitive, and will updare edge positions as well
   *
   */
  def updateNodePositions(graph:Graph) {
    nodePositionLayer = graph.position
    updateEdgePositions(graph)
  }

  /**
   * Update the nodes' colors layer
   *
   * This update is transitive, and will updare edge colors as well
   *
   */
  def updateNodeColors(graph:Graph) {

    nodeColorLayer = graph.selected.zipWithIndex map {
      case (s,i) =>
        graph category i match {
          case "Document" => 
            s match {
              case true => colors.primary.dark
              case false => colors.primary.light
            }
          case "NGram" => 
            s match {
              case true => colors.secondary.dark
              case false => colors.secondary.light
            }

          case other =>
            s match {
              case true => colors.tertiary.dark
              case false => colors.tertiary.light
            }
        }
    }

    nodeBorderColorLayer = graph.selected.zipWithIndex map {
      case (s,i) =>
        graph category i match {
          case "Document" => 
            s match {
              case true => colors.primary.darker
              case false => colors.primary.standard
            }
          case "NGram" => 
            s match {
              case true => colors.secondary.darker
              case false => colors.secondary.standard
            }

          case other =>
            s match {
              case true => colors.tertiary.darker
              case false => colors.tertiary.standard
            }
        }
       
    }
    updateEdgeColors(graph)
  }

  /**
   * Update the nodes' shapes layer
   *
   * By default, we take the node category and try to map it to
   */
  def updateNodeShapes(graph:Graph) {
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
  def updateNodeSizes(graph:Graph) {
    nodeSizeLayer = graph.weight
    updateEdgeColors(graph)
  }

  /**
   * Update the nodes' labels layer
   *
   *
   */
  def updateNodeLabels(graph:Graph) {
    nodeLabelLayer = graph.label
  }

  /**
   * Update the edges' positions layer
   *
   */
  private def updateEdgePositions(graph:Graph) {
    var source = -1

    //var tmpEdges = List.empty[((Double,Double),(Double,Double),Double,Color)]
    var tmpPosition = List.empty[((Double,Double),(Double,Double))]
    var tmpColor = List.empty[Color]
    var tmpWeight = List.empty[Double]

    //println("updateEdgePositions of "+graph)

    val tmpNodes = graph.linkIdArray.zipWithIndex map {
      case (links,i) =>
        links.zipWithIndex foreach {
          case (j,_j) =>
            val src = graph position i
            val trg = graph position j
            val weight = graph.linkWeightArray(i)(_j)
            val color = nodeColorLayer(i).blend(nodeColorLayer(j))
            tmpPosition ::= (src,trg)
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
  def updateEdgeColors(graph:Graph) {
    var tmpColor = List.empty[Color]

    graph.linkIdArray.zipWithIndex map {
      case (links,from) =>
        links foreach {
          case to => tmpColor ::= nodeColorLayer(from).blend(nodeColorLayer(to))
        }
    }
    edgeColorLayer = tmpColor.toArray
  }

  /**
   * Update the edges' colors layer
   *
   */
  def updateEdgeSizes(graph:Graph) {
    edgeWeightLayer = (for (weights <- graph.linkWeightArray; weight <- weights) yield weight).toArray
  }

}
