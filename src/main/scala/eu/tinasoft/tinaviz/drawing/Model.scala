/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.drawing

class NodeDrawing (val position:(Double,Double)=(0,0),
                   val radius:Double=1,
                   val color:(Int,Int,Int)=(0,0,0),
val shape:Symbol='Disk) {

}

class EdgeDrawing (val sourcePosition:(Double,Double)=(0,0),
                   val targetPosition:(Double,Double)=(0,0),
                   val weight:Double=1,
                   val color:(Int,Int,Int)=(0,0,0)) {


}

class LabelDrawing (val labelShort:String="N",
                    val labelFull:String="Node",
                    val size:Int=14) {

}

case class Model (
  val background : (Int,Int,Int) = (0,0,0),
  val debug : Boolean = true
) {

  val nodes : List[NodeDrawing] = List.empty
  val edges : List[EdgeDrawing] = List.empty
  val labels : List[LabelDrawing] = List.empty
  val labelColor : (Int,Int,Int) = (0,0,0)
}
