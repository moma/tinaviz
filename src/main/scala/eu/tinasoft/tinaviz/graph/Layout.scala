/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import org.daizoru._
import eu.tinasoft._

import tinaviz.util.Vector._
import tinaviz.util.Maths

object Layout {

  /**
   * Compute the layout
   */
  def layout(g:Graph) : Graph = {
    val nbNodes = g.nbNodes
    if (nbNodes == 0) return g
    val barycenter = (0.0, 0.0) //g.get[(Double, Double)]("baryCenter")
    val GRAVITY = g.get[Double]("layout.gravity") // stronger means faster!
    val ATTRACTION = g.get[Double]("layout.attraction")
    val REPULSION = g.get[Double]("layout.repulsion") // (if (nbNodes > 0) nbNodes else 1)// should be divided by the nb of edges
    
    //println("running forceVector on "+nbNodes+" nodes")
    //if (g.activity < 0.005) return g + ("activity" -> 0.0)
    val cooling =1.0
    
    //var activ = 0.0
    val positions = g.position.zipWithIndex map {
      case (p1, i) =>
        var force = (0.0,0.0)
        val p1inDegree = g inDegree i
        val p1outDegree = g outDegree i
        val p1degree = p1inDegree + p1outDegree
        
        // GRAVITY
        force += p1.computeLessForce(80 * cooling, (0.0,0.0))
        
        // WEIGHT MAP
        val weightMap = Map[Int,Double](g.links(i).map{ case (a,b) => (a,b) }.toList : _*)

        // IF THE NODE HAS NEIGHBOURS
        if (p1degree > 0) {
          
          // WE CHECK WHAT TO WITH ALL OTHER NODES
          g.position.zipWithIndex foreach {
            case (p2, j) =>
              val p2inDegree = g inDegree j
              val p2outDegree = g outDegree j
              val p2degree = p2inDegree + p2outDegree
              val doIt = Maths.randomBool

              //"layout.attraction" -> 1.01,
              //"layout.repulsion" -> 1.5,
              //
              // todo: attract less if too close (will work for both gravity and node attraction)
              if (g.hasAnyLink(i, j)) {
                force += p1.computeLessForce(10 * weightMap(j) * cooling, p2)
              } else {
                // if (doIt) {
                if (p2degree > 0) {
                  force -= p1.computeLessForce(0.4 * cooling, p2)
                }
                // }
              }
          }
          
          // ELSE THE NODE IS SINGLE, PUT IT ON THE RING
        } else {
          // for the moment we only apply the standard gravity..
        }
       
        // random repulse
        /*
         if (Maths.random() < 0.05f) {
         val theta = 2 * math.Pi * Maths.random()
         (((math.cos(theta) - math.sin(theta))) * 100,
         ((math.cos(theta) + math.sin(theta))) * 100)
         } else {
         p1 + force
         }*/
        
        //println("p1: "+p1+" abs_f1: "+math.abs(force._1)+" abs_f2 "+math.abs(force._2)+" p1': "+(p1 + force))
        p1 + (if (math.abs(force._1) > 0.01) force._1 else { /*println("cancelling force in X ");*/ 0.0 },
              if (math.abs(force._2) > 0.01) force._2 else { /*println("cancelling force in Y ");*/ 0.0 })
    }
   
    g + ("position" -> positions)
  }
}
