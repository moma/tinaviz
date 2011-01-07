/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

case class NodeDrawing (val position:(Double,Double)=(0,0),
                   val size:Double=1,
                   val color:(Int,Int,Int)=(50,50,50),
                   val shape:Symbol='Disk) {

}

case class EdgeDrawing (val source:(Double,Double)=(0,0),
                   val target:(Double,Double)=(0,0),
                   val weight:Double=1,
                   val thickness:Double=1,
                   val color:(Int,Int,Int)=(150,150,150),
                   val lod:Int=16) {


}

case class LabelDrawing (val text:String="Node",
                    val size:Int=14) {

}

case class Scene (
  val background : (Int,Int,Int) =  (255,255,255),
  val foreground : (Int,Int,Int) = (0,0,0),
  val nodes : List[NodeDrawing] = List.empty[NodeDrawing],
  val edges : List[EdgeDrawing]  = List.empty[EdgeDrawing],
  val labels : List[LabelDrawing] = List.empty[LabelDrawing],
  val labelColor : (Int,Int,Int) = (0,0,0)
) {
  
}

