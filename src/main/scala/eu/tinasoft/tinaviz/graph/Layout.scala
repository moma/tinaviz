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
      case (nodePosition, i) =>
        var delta = (0.0,0.0)
        val p1inDegree = g inDegree i
        val p1outDegree = g outDegree i
        val p1degree = p1inDegree + p1outDegree
        
        // GRAVITY
        delta += nodePosition.computeForce(GRAVITY * cooling, barycenter)
        
        // WEIGHT MAP
        val weightMap = Map[Int,Double](g.links(i).map{ case (a,b) => (a,b) }.toList : _*)

        // IF THE NODE HAS NEIGHBOURS
        if (p1degree > 0) {
          
          // WE CHECK WHAT TO DO FOR EACH OTHER NODE
          g.position.zipWithIndex foreach {
            case (otherNodePosition, j) =>
              val p2inDegree = g inDegree j
              val p2outDegree = g outDegree j
              val p2degree = p2inDegree + p2outDegree
              val doIt = Maths.randomBool

              // FONCTION UTILITAIRES
              // g.hasAnyLink(A,B)  A <--> B
              // g.hasThisLink(A,B)  A --> B
              
              // AVOIR UN ATTRIBUT
              // val occ = g.get[Double]("occurences")(ID_DU_NOEUD)
              // val occ = g.get[String]("category")(ID_DU_NOEUD)
              // 
                // if we have a link to another node..
                if (weightMap.contains(j)) {
                  // we go toward it..
                 // delta += nodePosition.computeForce(ATTRACTION * weightMap(j) * cooling, otherNodePosition)
                } else  {
                  // else we escape!
                delta -= nodePosition.computeForce(REPULSION * cooling, otherNodePosition)
                }
          }
          
        } else {
          // else put on the ring?
        }
       
        // random repulse
        /*
         if (Maths.random() < 0.05f) {
         val theta = 2 * math.Pi * Maths.random()
         (((math.cos(theta) - math.sin(theta))) * 100,
         ((math.cos(theta) + math.sin(theta))) * 100)
         } else {
         p1 + delta
         }*/
        
        //println("p1: "+p1+" abs_f1: "+math.abs(delta._1)+" abs_f2 "+math.abs(delta._2)+" p1': "+(p1 + delta))
        // try to not apply force 
        nodePosition + (if (math.abs(delta._1) > 0.01) delta._1 else { 0.0 },
                        if (math.abs(delta._2) > 0.01) delta._2 else { 0.0 })
    }
   
    g + ("position" -> positions)
  }
}
