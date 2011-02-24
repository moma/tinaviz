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
    val barycenter = g.get[(Double, Double)]("baryCenter")
    val minMaxWeights = (g.get[Double]("minEdgeWeight"),g.get[Double]("maxEdgeWeight"))
    
  
    val GRAVITY = 30 // g.get[Double]("layout.gravity") // stronger means faster!
    val ATTRACTION = g.get[Double]("layout.attraction")
    val REPULSION = g.get[Double]("layout.repulsion") // (if (nbNodes > 0) nbNodes else 1)// should be divided by the nb of edges
    
    //println("running forceVector on "+nbNodes+" nodes")
    //if (g.activity < 0.005) return g + ("activity" -> 0.0)
    val cooling =1.0
    
    //var activ = 0.0
    val deltas = g.position.zipWithIndex map {
      case (nodePosition, i) =>
        var delta = (0.0,0.0)
        val p1inDegree = g inDegree i
        val p1outDegree = g outDegree i
        val p1degree = p1inDegree + p1outDegree

        // GRAVITY
        delta += nodePosition.attractLess( 1.5 * cooling, barycenter).absLimit((0.01,100),(0.0,100))
        
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
                  val ponderatedWeight = Maths.map(weightMap(j),minMaxWeights,(0.5,1.0))
                  delta += nodePosition.attractLess(0.01 *  cooling, otherNodePosition).absLimit((0.01,100),(0.0,100))
                } else  {
                  // else we escape!
                 
               delta -= nodePosition.attractLess(10 * cooling, otherNodePosition).absLimit((0.01,100),(0.0,100))
                }
          }
          
        } else {
          // else put on the ring?
        }
        
        // absolute value limiter. if < 0.01, we map to 0.0, if > 100 we map to 100
        // negative values are limited too (eg. -150 will become -100 as well)
        delta//.absLimit((0.01,100),(0.0,100))

    }
       
    //var activ = 0.0
    val positions = g.position zip deltas map {
      case (a,b) => a + b
    }
    g + ("position" -> positions)
  }
}
