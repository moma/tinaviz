/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

class MutableScene (
  val background : (Int,Int,Int) =  (255,255,255),
  val foreground : (Int,Int,Int) = (0,0,0),
  val debug : Boolean = true,
  val paused : Boolean = false
) {

  val nodes : List[NodeDrawing] = List.empty
  val edges : List[EdgeDrawing] = List.empty
  val labels : List[LabelDrawing] = List.empty
  val labelColor : (Int,Int,Int) = (0,0,0)
  
  def toScene : Scene = {
    val scene = new Scene(
      background,
      foreground,
      debug,
      paused
    )
  }
}
