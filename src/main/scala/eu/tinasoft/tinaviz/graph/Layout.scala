/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.graph

import org.daizoru._
import eu.tinasoft._
import traer.physics._

import tinaviz.util.Vector._
import tinaviz.util.Maths

object Layout {
  
  val ps = new ParticleSystem(0f, 0.1f)

  ps.setIntegrator( ParticleSystem.MODIFIED_EULER )
  //ps.setGravity( 0.0f ) // between 0.0 and 5.0
  
  var gravity = ps.makeParticle
  gravity.position().set( 0.0f,0.0f,0.0f )
  gravity.setMass( 1.0f )
  gravity.makeFixed
  
  var lastHash = 0

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
    
     val factor = 100000.0
     

          
    if (g.hashed != lastHash) {
      lastHash = g.hashed
      println("hash changed, regenerating a particle system..")
      
      gravity = ps.makeParticle
      gravity.position().set( barycenter._1.toFloat,barycenter._2.toFloat, 0.0f )
      gravity.setMass( 1.0f )
      gravity.makeFixed
  
    val positions = g.position.zipWithIndex map {
       case (nodePosition, i) =>
         ((nodePosition._1 * factor, nodePosition._2*factor), i)
    }
      // nodes
      val tmp = positions map {
      case (nodePosition, i) =>
        //val p1inDegree = g inDegree i
        //val p1outDegree = g outDegree i
        //val p1degree = p1inDegree + p1outDegree
        // g.weight(i).toFloat
          val p = ps.makeParticle( 1.0f, nodePosition._1.toFloat, nodePosition._2.toFloat, 0.0f )
         //ps.makeSpring(gravity, p, 0.3f, 1.0f, 1.0f)    
        (nodePosition, p, i)
      } 
      
      // every node are repulsing each other
      val tmp2 = tmp.map {
        case (node1, p1, i) =>
        tmp map {
          case (node2, p2, j) =>
          //if (j > i) ps.makeSpring(p1, p2, 20, 20.0f, 100f)
        }
        (node1, p1,  Map[Particle,Double](g.links(i).map{ case (a,b) => (tmp(a)._2,b) }.toList : _*), i)
      }
      
      // links are attrazcting with a spring
      tmp2 foreach {
        case (nodePosition, src, links, i) =>

           links.foreach {
             case (target, weight) =>
                // val ponderatedWeight = Maths.map(weightMap(j),minMaxWeights,(40.0,60.0))
               ps.makeSpring(src, target, 0.3f, 1.0f, 1.0f)
           }
      }
    }
    
    println("running step ("+ps.numberOfParticles+" particles)..")
    ps.tick(1.0f)
    
    //var activ = 0.0
   g + ("position" -> (g.position.zipWithIndex map {
      case (nodePosition, i) =>
       val v = ps.getParticle( i+1 )
       (v.position().x().toDouble / factor, v.position().y().toDouble / factor)
        
    }))
  }
}
