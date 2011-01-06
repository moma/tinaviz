/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.drawing

import processing.core.PApplet

/**
 *
 * @author jbilcke
 */
class Fonts(val p : PApplet,
            val fontName : String = "Arial",
            val size:Int=80,
            val defaultFontSize : Int = 12) {

  val fonts = for (i <- List.range(1,size))
    yield p.createFont(fontName, i, true)

  val defaultFont = fonts(defaultFontSize)   
    
  def get(s:Int) = fonts (if (s > 1) (if (s < size) s else size) else 1)
}

