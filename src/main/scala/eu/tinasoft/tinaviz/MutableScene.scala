/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz

class MutableScene (
  var background : (Int,Int,Int) =  (255,255,255),
  var foreground : (Int,Int,Int) = (0,0,0),
  var nodes : List[NodeDrawing] = List[NodeDrawing](),
  var edges : List[EdgeDrawing]  = List[EdgeDrawing](),
  var labels : List[LabelDrawing] = List[LabelDrawing](),
  var labelColor : (Int,Int,Int) = (0,0,0)
  ) {
  
  def toScene = new Scene(
    background,
    foreground,
    nodes,
    edges,
    labels,
    labelColor 
  )

}
