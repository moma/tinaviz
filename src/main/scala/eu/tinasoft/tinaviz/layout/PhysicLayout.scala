/************************************************************************
                                  Tinaviz
 *************************************************************************
This application is part of the Tinasoft project: http://tinasoft.eu
 Tinaviz main developer: julian.bilcke @ iscpif.fr  (twitter.com/flngr)

Copyright (C) 2009-2011 CREA Lab, CNRS/Ecole Polytechnique UMR 7656 (Fr)
 ************************************************************************/

package eu.tinasoft.tinaviz.layout

import eu.tinasoft._
import traer.physics._

import tinaviz.util.Vector._
import tinaviz.util.Maths
import tinaviz.graph._

object PhysicLayout {
  val ps = new ParticleSystem(0f, 0.1f)
  //ps.setIntegrator( ParticleSystem.MODIFIED_EULER )
  //ps.setGravity( 1.3f )
  //ps.setDrag(0.1f)

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

    val GRAVITY = 200 // g.get[Double]("layout.gravity") // stronger means faster!
    val REPULSION = 800 // should be divided by the nb of edges? faster at the beggining, then slower?
    val DAMPING = 0.002 // please, no greater than 0.05
    val STRENGTH = 0.05 // 0.05 looks perfect
    val maxLinkLength = 80 // max distance between linked nodes
    val minLinkLength = 5 // min distance between linked nodes
    val minDistance = 5 // min distance between unlinked nodes (and thus clusters)

    //since I can't normalize weight, it seems I have to adapt the drag myself
    //ps.setDrag((if (g.nbEdges > 20000) 0.2 else 0.4).toFloat)

    val positionIndexSingle = g.position.zipWithIndex map {
      case (p, i) => (p, i, g.isSingle(i))
    }

    if (g.hashed != lastHash) {
      lastHash = g.hashed
      ps.clear
      //println("hash changed, regenerating the particle system..")

      val aMinMaxWeights = (g.minAEdgeWeight, g.maxAEdgeWeight)
      val bMinMaxWeights = (g.minBEdgeWeight, g.maxBEdgeWeight)

      //val nbEdges = g.nbEdges.toDouble / 2.0
      //val springFactor = if (nbEdges > 20000) 0.005f else 0.01f

      val distInterval = (minLinkLength.toDouble, maxLinkLength.toDouble)

      val gravity = ps.makeParticle(GRAVITY.toFloat, 0.0f, 0.0f, 0.0f)
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
                  // val strictDistance = (g.size(i1) + g.size(i2))
                  // val securityDistance = (strictDistance * 1.20) * g.cameraZoom // 20%
                  val w = g.links(i1)(i2)
                  // if we have a link, we create a spring
                  val minMaxInterval = (g.category(i1), g.category(i2)) match {
                    case ("Document", "Document") => aMinMaxWeights
                    case ("NGram", "NGram") => bMinMaxWeights
                    case any => (w - 1.0, w + 1.0)
                  }
                  ps.makeSpring(
                    p1,
                    p2,
                    STRENGTH.toFloat,
                    DAMPING.toFloat,
                    (Maths.map(w, minMaxInterval, (0.0, 1.0)) match {
                        case l => ((1.0 - l) * (distInterval._2 - distInterval._1)) + distInterval._1
                    }).toFloat)
                } else if (!g.hasAnyLink(i1, i2)) ps.makeAttraction(p1, p2, -REPULSION.toFloat, minDistance.toFloat) // default -600   we repulse unrelated nodes
              }
          }

          ps.makeAttraction(p1, gravity, 1500f, 200f) // apply the gravity
      }
    } // end hash changed

    // fix the center
    ps.getParticle(0).position().set(0.0f, 0.0f, 0.0f)

    // effectively run one step of simulation
    ps.tick(1.0f)

    val gDiameter = Metrics.notSingleNodesDimension(g) match {
      case dim => math.max(dim._1, dim._2) * 0.7
    }
    var (ci, cj) = (0, 0)

    if (g.pause) g
    else {
      g + ("position" -> (positionIndexSingle map {
        case (nodePosition, i, s) =>

        // if we have at least one single
          if (s) {
            ci += 1
            if (gDiameter > 0) {
              // we want to use non single nodes as reference for the circle
              (g.notSinglesCenter._1 + gDiameter * math.cos(math.Pi / 2 + 2 * math.Pi * ci / g.nbSingles),
                g.notSinglesCenter._2 + gDiameter * math.sin(math.Pi / 2 + 2 * math.Pi * ci / g.nbSingles))
            } else {
              // if we can't we will use an arbitrary, fixed-length
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
