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
    updateNodePositions(graph)
    updateNodeColors(graph)
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
    nodePositionLayer = graph.get[(Double,Double)]("position")
    updateEdgePositions(graph)
  }

  /**
   * Update the nodes' colors layer
   *
   * This update is transitive, and will updare edge colors as well
   *
   */
  def updateNodeColors(graph:Graph) {

    var i = -1
    nodeColorLayer = graph.get[Boolean]("selected").map { case s => i += 1
        graph.get[String]("category")(i) match {
          case "Document" => 
            s match {
              case true => colors.primary.dark
              case false => colors.primary.standard
            }
          case "NGram" => 
            s match {
              case true => colors.secondary.dark
              case false => colors.secondary.standard
            }
        }
    }

    i = -1
    nodeBorderColorLayer = graph.get[Boolean]("selected").map{ case s => i += 1
        graph.get[String]("category")(i) match {
          case "Document" => 
            s match {
              case true => new Color(1.0,1.0,0.0)
              case false => colors.primary.darker
            }
          case "NGram" => 
            s match {
              case true => new Color(1.0,1.0,0.0)
              case false => colors.primary.darker
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
    var i = -1
    val tmpNodes = graph.nodes.map{
      case n =>
        i += 1
        val j = i
        (n,j)
    }
    var colorTmp = List.empty[Color]
    var t = for (nodeAndId <- tmpNodes;link <- nodeAndId._1.links)
      yield {
        val node = nodeAndId._1
        val nodeId = nodeAndId._2
        val neighbourId = link._1
        val neighbour = graph.node(neighbourId)
   
        val edgeAB = (node, neighbour)
        val edgeBA = (neighbour, node)
        
        val edge = {
          // neighbour has a link in our direction
          if (neighbour.hasLink(nodeId)) {
            val nodeWeight = node.get[Double]("weight") 
            val neighbourWeight = neighbour.get[Double]("weight")
            if (nodeWeight == neighbourWeight) {
              if (node.label > neighbour.label) edgeAB else edgeBA
            } else {
              if (nodeWeight > neighbourWeight) edgeAB else edgeBA
            }
          } else {
            edgeAB
          }
        }

        colorTmp ::= computeColor(node).blend(computeColor(neighbour))
                                              
        (edge._1.position, edge._2.position)
      }
     
    edgePositionLayer = t.toArray
    edgeColorLayer = colorTmp.toArray
  }


  /**
   * Update the edges' colors layer
   *
   */
  def updateEdgeColors(graph:Graph) {
    var t = for (node <- graph.nodes; link <- node.links)
      yield computeColor(node).blend(computeColor(graph.node(link._1)))
    edgeColorLayer = t.toArray
  }


}
