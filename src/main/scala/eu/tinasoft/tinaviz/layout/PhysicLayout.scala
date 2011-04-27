/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.layout

import eu.tinasoft._
import traer.physics._

import tinaviz.util.Vector._
import tinaviz.util.Maths
import tinaviz.graph._

object PhysicLayout {

  val ps = new ParticleSystem(0f, 0.1f)

  //ps.setIntegrator( ParticleSystem.MODIFIED_EULER )
  //
  // ps.setGravity( 1.3f ) // between 0.0 and 5.0
  //ps.setDrag(0.1f) // was 0.1 before

  var lastHash = 0


  def layout(g: Graph): Graph = {
    g.layout match {
      case "tinaforce" => tinaforce(g)
      case any =>
        g.currentView match {
          case "macro" =>
            g.currentCategory match {
              case "NGram" => tinaforce(g)
              case "Document" => tinaforce(g)
            }
          case "meso" =>
            g.currentCategory match {
              case "NGram" => phyloforce(g)
              case "Document" => phyloforce(g)
            }
        }
    }
  }

  /**
   * Compute the layout
   */
  def tinaforce(g: Graph): Graph = {
    if (g.nbNodes == 0) return g
    val springFactor = if (g.nbEdges > 20000) {
      0.005f
    } else {
      0.01f
    } // 0.02 is better..

    //since I can't normalize weight, it seems I have to adapt the drag myself
    val drag = if (g.nbEdges > 20000) {
      0.2
    } else {
      0.4
    }
    //println("setting drag to " + drag)
    //ps.setDrag(drag.toFloat)

    val aMinMaxWeights = (g.minAEdgeWeight, g.maxAEdgeWeight)
    val bMinMaxWeights = (g.minBEdgeWeight, g.maxBEdgeWeight)

    val GRAVITY = 30 // g.get[Double]("layout.gravity") // stronger means faster!
    val ATTRACTION = g.get[Double]("layout.attraction")
    val REPULSION = g.get[Double]("layout.repulsion") // (if (nbNodes > 0) nbNodes else 1)// should be divided by the nb of edges

    val nbEdges = g.nbEdges.toDouble / 2.0
    //println("nbEdges: "+nbEdges)
    val maxD = 50.0 // 50 seems too big
    val minD = 32.0 // min distance . 8 seems to be too short with default node size and zoom settings
    val maxEdges = 3000.0

    // between min (16) and max (20~50)
    //val distInterval = (if (nbEdges > maxEdges) maxD else Maths.map(nbEdges, (0.0, maxEdges), (32, maxD)), minD)
    val distInterval = (100.0,20.0) // 200, 80 seems good!
    //println("distInterval: "+distInterval)

    //val distInterval = (if (nbEdges > maxEdges) maxD else Maths.map(nbEdges, (0.0, maxEdges), (12.0, maxD)), minD)

    //println("running forceVector on "+nbNodes+" nodes")
    //if (g.activity < 0.005) return g + ("activity" -> 0.0)
    val cooling = 1.0
    val positionIndexSingle = g.position.zipWithIndex map {
      case (p, i) => (p, i, g.isSingle(i))
    }

    // ou si on change l'attribut taille
    if (g.hashed != lastHash) {
      lastHash = g.hashed
      //println("hash changed, regenerating a particle system..")
      //g.position.zipWithIndex map {

      // }

      ps.clear // we clean everything, actually.. this could be optimized
      val gravity = ps.makeParticle(200.0f, 0.0f, 0.0f, 0.0f)
      gravity.makeFixed

      val positionIndexNotSingleParticle = positionIndexSingle.filter {
        case (p, i, s) => !s // on garde si on est pas single
      }.map {
        case ((x, y), i, s) => ((x, y), i, ps.makeParticle(1.0f, x.toFloat, y.toFloat, 0.0f))
      }

      // every node are repulsing each other (negative attraction)
      positionIndexNotSingleParticle foreach {
        case (pos1, i1, p1) =>
          positionIndexNotSingleParticle foreach {
            case (pos2, i2, p2) =>
              if (i2 != i1) {
                if (g.hasThisLink(i1, i2)) {
                  // if we have a link, we create a spring
                  val minMaxInterval = g.category(i1) match {
                    case "Document" => aMinMaxWeights
                    case "NGram" => bMinMaxWeights
                  }

                  // val strictDistance = (g.size(i1) + g.size(i2))
                  // val securityDistance = (strictDistance * 1.20) * g.cameraZoom // 20%

                  // Rest Length - the spring wants to be at this length and acts on the particles to push or pull them exactly this far apart at all times.
                  // we want dist interval to be [50,30]
                  val l = Maths.map(g.links(i1)(i2), minMaxInterval, distInterval)
                  //val l = 80
                  //val l = (Maths.map(g.links(i1)(i2), minMaxInterval, distInterval) match { case l => if (l < securityDistance) securityDistance else l })
                   //println("("+g.label(i1)+" -> "+g.label(i2)+") securityDistance: "+securityDistance+"    l:"+l+ "distInterval: "+distInterval)
                   //val l = 150


                  // Strength
                  // "If they are strong they act like a stick. If they are weak they take a long time to
                  // return to their rest length."
                  // 0.05 seems to be a good value - lower values okay, but greater looks very unstable!
                  val s = 0.05

                  // Damping - If springs have high damping they don't overshoot and they settle down quickly, with low damping springs oscillate.
                  //val d = Maths.map(g.links(i1)(i2), minMaxInterval, (0.01, 0.015))
                  val d = 0.05 // 0.015

                  ps.makeSpring(p1, p2, s.toFloat, d.toFloat, l.toFloat) // 10.0f (float strength, float damping, float restLength)
                }
                else if (!g.hasAnyLink(i1, i2)) ps.makeAttraction(p1, p2, -500f, 10f) // default -600   we repulse unrelated nodes
              }
          }
          ps.makeAttraction(p1, gravity, 900f, 200f) // apply the gravity
      }
    } // end hash changed

    // if a position has changed
    /*
    positionIndexSingleParticle.filter(case (pos, i, s, p) => !s).foreach {
      case (pos, i, s, p) =>
        val (x, y, z) = (Maths.limit(pos._1, -2000, 2000).toFloat,
                         Maths.limit(pos._2,-2000,2000).toFloat,
                         0.0f)
        val p = ps.getParticle(i).position()
        if (p.x != x || p.y != y) p.set(x, y, z)
    }*/


    // fix the center

    //ps.getParticle(0).position().set(g.notSinglesCenter._1.toFloat, g.notSinglesCenter._2.toFloat, 0.0f)
    //ps.getParticle(0).position().set(g.baryCenter._1.toFloat, g.baryCenter._2.toFloat, 0.0f)

    ps.getParticle(0).position().set(0.0f, 0.0f, 0.0f)

    //println("running step (" + ps.numberOfParticles + " particles)..")
    ps.tick(1.0f)
    val dim = Metrics.notSingleNodesDimension(g)

    val gDiameter = math.max(dim._1, dim._2) * 0.6
    //var activ = 0.0
    var ci = 0
    var cj = 0

    val h = if (g.pause) g
    else {
      g + ("position" -> (positionIndexSingle map {
        case (nodePosition, i, s) =>

        // if we have at least one single
          if (s) {
            ci += 1

              // we want to use non single nodes as reference for the circle
              if (gDiameter > 0) {
                (g.notSinglesCenter._1 + gDiameter * math.cos(math.Pi / 2 + 2 * math.Pi * ci / g.nbSingles),
                  g.notSinglesCenter._2 + gDiameter * math.sin(math.Pi / 2 + 2 * math.Pi * ci / g.nbSingles))
              }

              // if we can't we will use an arbitrary, fixed-length
              else {
                (g.notSinglesCenter._1 + 100.0 * math.cos(math.Pi / 2 + 2 * math.Pi * ci / g.nbSingles),
                  g.notSinglesCenter._2 + 100.0 * math.sin(math.Pi / 2 + 2 * math.Pi * ci / g.nbSingles))
              }
          }

          // if the node is not single, then we can safely get it's coordinates from the particle engline
          else if (!s) {
            cj += 1 // okay to not start with zero here, because slot 0 is already used by gravity
            val p = ps.getParticle(cj).position()
            p.setX(Maths.limit(p.x().toDouble, -2000, 2000).toFloat)
            p.setY(Maths.limit(p.y().toDouble, -2000, 2000).toFloat)

            val (x, y) = (p.x().toDouble, p.y().toDouble)
            val v = ps.getParticle(cj).velocity()
            v.setX(Maths.limit(v.x().toDouble, -50, 50).toFloat)
            v.setY(Maths.limit(v.y().toDouble, -50, 50).toFloat)

            (x, y)
          } else {
            // node is not single
            nodePosition
          }
      }))
    }

    /*
    h + ("position" -> (h.position.zipWithIndex map {
      case (position, i) =>
        val r = (position._1 - h.baryCenter._1, position._2 - h.baryCenter._2)

        // now we "patch" the physical engine to fix the positions
        val p = ps.getParticle(cj).position()
        p.setX(r._1.toFloat)
        p.setY(r._2.toFloat)
        r
    }))  */
    h
  }

  /**
   * Compute the layout
   */
  def phyloforce(g: Graph): Graph = {

    if (g.nbNodes == 0) return g

    val springFactor = if (g.nbEdges > 20000) {
      0.002f
    } else {
      0.02f
    }
    val drag = if (g.nbEdges > 20000) {
      0.3
    } else {
      0.2
    }
    //println("setting drag to " + drag)
    ps.setDrag(drag.toFloat)

    val aMinMaxWeights = (g.minAEdgeWeight, g.maxAEdgeWeight)
    val bMinMaxWeights = (g.minBEdgeWeight, g.maxBEdgeWeight)

    val GRAVITY = 30 // g.get[Double]("layout.gravity") // stronger means faster!
    val ATTRACTION = g.get[Double]("layout.attraction")
    val REPULSION = g.get[Double]("layout.repulsion") // (if (nbNodes > 0) nbNodes else 1)// should be divided by the nb of edges

    //println("running forceVector on "+nbNodes+" nodes")
    //if (g.activity < 0.005) return g + ("activity" -> 0.0)
    val cooling = 1.0

    if (g.hashed != lastHash) {
      lastHash = g.hashed
      //println("hash changed, regenerating a particle system..")

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
          val p = ps.makeParticle(1.0f, node1._1.toFloat, node1._2.toFloat, 0.0f)
          //ps.makeSpring(gravity, p, 0.3f, 1.0f, 1.0f)
          (p, i)
      }

      // every node are repulsing each other (negative attraction)
      tmp foreach {
        case (p1, i) =>
          tmp foreach {
            case (p2, j) =>
              if (j != i) {
                // if we have a link, we create a sprinf
                if (g.hasThisLink(i, j)) {
                  val d = Maths.map(g.links(i)(j), g.category(i) match {
                    case "Document" => aMinMaxWeights
                    case "NGram" => bMinMaxWeights

                  }, (3.0, 3.5)) // seems pretty small..
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
        val (x, y, z) = (nodePosition._1.toFloat,
          nodePosition._2.toFloat,
          0.0f)
        val p = ps.getParticle(i).position()
        if (p.x != x || p.y != y) p.set(x, y, z)
    }

    //println("running step (" + ps.numberOfParticles + " particles)..")
    ps.tick(1.0f)

    //var activ = 0.0
    g + ("position" -> (g.position.zipWithIndex map {
      case (nodePosition, i) =>
      //if (g.links(i).size > 0) {
        val v = ps.getParticle(i)
        v.position().setY(nodePosition._2.toFloat)
        (v.position().x().toDouble, nodePosition._2)
      //} else {
      //  (0.0, 0.0)
      //}
    }))
  }
}
