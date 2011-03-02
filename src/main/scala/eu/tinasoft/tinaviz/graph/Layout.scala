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

  //ps.setIntegrator( ParticleSystem.MODIFIED_EULER )
  //
  //ps.setGravity( 0.0f ) // between 0.0 and 5.0
  ps.setDrag( 0.1f )
  
  
  var gravity = ps.makeParticle
  gravity.position().set( 0.0f,0.0f,0.0f )
  gravity.setMass( 1.0f )
  gravity.makeFixed
  
  var lastHash = 0

  /**
   * Compute the layout
   */
  def layout(g:Graph) : Graph = {

    if (g.nbNodes == 0) return g
    
    val springFactor = if (g.nbEdges > 20000) {
      0.002f
    } else {
      0.02f
    }
    val drag = if (g.nbEdges > 20000) {
     0.01
    } else {
      0.1
    }
   println("setting drag to "+drag)
    ps.setDrag( drag.toFloat )
    val barycenter = g.get[(Double, Double)]("baryCenter")
    val minMaxWeights = (g.get[Double]("minEdgeWeight"),g.get[Double]("maxEdgeWeight"))
    
  
    val GRAVITY = 30 // g.get[Double]("layout.gravity") // stronger means faster!
    val ATTRACTION = g.get[Double]("layout.attraction")
    val REPULSION = g.get[Double]("layout.repulsion") // (if (nbNodes > 0) nbNodes else 1)// should be divided by the nb of edges
    
    //println("running forceVector on "+nbNodes+" nodes")
    //if (g.activity < 0.005) return g + ("activity" -> 0.0)
    val cooling =1.0

    if (g.hashed != lastHash) {
      lastHash = g.hashed
      println("hash changed, regenerating a particle system..")
      
      //g.position.zipWithIndex map {
        
     // }
      
      /*
       gravity = ps.makeParticle
      
       gravity.position().set( barycenter._1.toFloat,barycenter._2.toFloat, 0.0f )
       gravity.setMass( 1.0f )
       gravity.makeFixed
       */
      ps.clear
      val tmp = g.position.zipWithIndex map {
        case (node1, i) =>
          // g.weight(i).toFloat
          val p = ps.makeParticle( 1.0f, node1._1.toFloat, node1._2.toFloat, 0.0f )
          //ps.makeSpring(gravity, p, 0.3f, 1.0f, 1.0f)    
          (p, i)
      } 

      // every node are repulsing each other (negative attraction)
      tmp foreach { case (p1, i) =>
          tmp foreach { case (p2, j) =>
              if (j != i) {
                // if we have a link, we create a sprinf
                if (g.hasThisLink(i, j)) {
                  val d = Maths.map( g.links(i)(j), minMaxWeights, (3.0, 6.0))
                  //
                  ps.makeSpring(p1, p2, springFactor, springFactor, d.toFloat) // 10.0f
                }
                 // we repulse unrelated nodes
                else if (!g.hasAnyLink(i, j)) ps.makeAttraction(p1, p2, -1000f, 10f)
           
              }
          }
      }
    }
    
    // if a position has changed 
    // (eg. after a normalization), we update the engine
    g.position.zipWithIndex foreach {
          case (nodePosition, i) =>
            val (x,y,z) = (nodePosition._1.toFloat, 
                           nodePosition._2.toFloat, 
                           0.0f)
            val p = ps.getParticle( i ).position()
            if (p.x != x || p.y != y) p.set(x,y,z)
    }
    
    println("running step ("+ps.numberOfParticles+" particles)..")
    ps.tick(1.0f)
    
    //var activ = 0.0
    g + ("position" -> (g.position.zipWithIndex map {
          case (nodePosition, i) =>
            val v = ps.getParticle( i )
            (v.position().x().toDouble, v.position().y().toDouble)
        }))
  }
}
