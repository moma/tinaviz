/************************************************************************
                                  Tinaviz
 *************************************************************************
This application is part of the Tinasoft project: http://tinasoft.eu
 Tinaviz main developer: julian.bilcke @ iscpif.fr  (twitter.com/flngr)

Copyright (C) 2009-2011 CREA Lab, CNRS/Ecole Polytechnique UMR 7656 (Fr)
 ************************************************************************/

package eu.tinasoft.tinaviz.layout

import eu.tinasoft._
//import traer.physics._
import codeanticode.clphysics._ // OpenCL version of traer.physics! :)
import tinaviz.util.Vector._
import tinaviz.util.Maths
import tinaviz.graph._
import tinaviz.pipeline.Pipeline
import actors.threadpool.AbstractCollection

import scala.Math

object PhysicLayout {
  //new ParticleSystem(this, ParticleSystem.RUNGE_KUTTA, numBodies, POINTS);
  val ps = new ParticleSystem()
  //ps.setIntegrator( ParticleSystem.MODIFIED_EULER )
  //ps.setGravity( 1.3f )
  //ps.setDrag(0.1f)

  var lastHash = 0

  def layout(g: Graph): Graph = {

    g.layout match {
      case "tinaforce" => tinaforce(g)
      case any =>
        phyloforce(g)
        /*
        g.currentView match {
          case "macro" =>
            g.currentCategory match {
              case "NGram" => tinaforce(g)
              case "Document" => phyloforce(g)
            }
          case "meso" =>
            g.currentCategory match {
              case "NGram" => tinaforce(g)
              case "Document" => tinaforce(g)
            }

             case any =>
            g.currentCategory match {
              case "NGram" => tinaforce(g)
              case "Document" => tinaforce(g)
            }
        }
        */
    }
  }

  /**
   * Compute the layout
   */
  def tinaforce(g: Graph): Graph = {
    if (g.nbNodes == 0) return g

    val h = Pipeline.categoryCache

    val GRAVITY = 200 // 200    g.get[Double]("layout.gravity") // stronger means faster!
    val REPULSION = 900 // 800    should be divided by the nb of edges? faster at the beggining, then slower?
    val DAMPING = 0.002 // 0.002  please, no greater than 0.05
    val STRENGTH = 0.03 // 0.05   looks perfect on 90% of the graphs.. but 10% need 0.03 :/
    val maxLinkLength = 70 // 80     max distance between linked nodes
    val minLinkLength = 16 // 5      min distance between linked nodes
    val minDistance = 16 // 5      min distance between unlinked nodes (and thus clusters)


    var deltas = 0

    //since I can't normalize weight, it seems I have to adapt the drag myself
    //ps.setDrag((if (g.nbEdges > 20000) 0.2 else 0.4).toFloat)

    val positionIndexSingle = g.position.zipWithIndex map {
      case (p, i) => (p, i, g.isSingle(i))
    }
    //println("Layout: position.size: "+g.position.size)
    if (g.hashed != lastHash) {
      lastHash = g.hashed
      ps.clear
      //println("hash changed, regenerating the particle system..")

      val aMinMaxWeights = (h.minAEdgeWeight, h.maxAEdgeWeight)
      val bMinMaxWeights = (h.minBEdgeWeight, h.maxBEdgeWeight)
      val aMinMaxNodeWeights = (g.minANodeWeight, g.maxANodeWeight)
      val bMinMaxNodeWeights = (g.minBNodeWeight, g.maxBNodeWeight)

      //println("Detected min/max for Documents --> Nodes: "+aMinMaxNodeWeights+"  Links: "+aMinMaxWeights)
      //println("Detected min/max for NGrams    --> Nodes: "+bMinMaxNodeWeights+"  Links: "+bMinMaxWeights)
      //val nbEdges = g.nbEdges.toDouble / 2.0
      //val springFactor = if (nbEdges > 20000) 0.005f else 0.01f

      //val distInterval = (minLinkLength.toDouble, maxLinkLength.toDouble)

      val gravity = ps.makeParticle(GRAVITY.toFloat, 0.0f, 0.0f, 0.0f)
      gravity.makeFixed

      val positionIndexNotSingleParticle = positionIndexSingle.filter {
        case (p, i, s) => !s // on garde si on est pas single
      }.map {
        case ((x, y), i, s) => ((x, y), i, ps.makeParticle(1.0f, x.toFloat, y.toFloat, 0.0f))
      }

      // every node are repulsing each other (negative attraction)
      positionIndexNotSingleParticle.foreach {
        case (pos1, i1, p1) =>
          positionIndexNotSingleParticle.foreach {
            case (pos2, i2, p2) =>
              if (i2 != i1) {

                val strictDistance = ((g.size(i1) / 2.0) + (g.size(i2) / 2.0))
                val securityDistance = (strictDistance * 1.20) // 20%
                val distInterval = (minLinkLength.toDouble, maxLinkLength.toDouble)

                if (g.hasThisLink(i1, i2)) {

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
                    Maths.map(w, minMaxInterval, (0.02, 0.05)).toFloat,
                    //STRENGTH.toFloat,
                    DAMPING.toFloat,
                    (Maths.map(w, minMaxInterval, (0.0, 1.0)) match {
                      case l => ((1.0 - l) * (distInterval._2 - distInterval._1)) + distInterval._1
                    }).toFloat)
                } else if (!g.hasAnyLink(i1, i2)) ps.makeAttraction(p1, p2, -REPULSION.toFloat, (g.size(i1) / 2.0).toFloat) // default -600   we repulse unrelated nodes
              }
          }

          ps.makeAttraction(p1, gravity, 2000f, 400f) // apply the gravity
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

    if (g.pause) { Thread.sleep(1000); g }
    else {
      g + ("position" -> (positionIndexSingle.map {
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
            val (oldX, oldY) = (nodePosition._1, nodePosition._2)
            val deltaLocal = (math.abs(oldX - x), math.abs(oldY - y))
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

    val h = Pipeline.categoryCache

    val GRAVITY = 200 // 200    g.get[Double]("layout.gravity") // stronger means faster!
    val REPULSION = 1800 // 800    should be divided by the nb of edges? faster at the beggining, then slower?
    val DAMPING = 0.05 // 0.002  please, no greater than 0.05
    val STRENGTH = 0.05 // 0.05   looks perfect on 90% of the graphs.. but 10% need 0.03 :/
    val maxLinkLength = 30 // 80     max distance between linked nodes
    val minLinkLength = 16 // 5      min distance between linked nodes
    val minDistance = 16 // 5      min distance between unlinked nodes (and thus clusters)

    //since I can't normalize weight, it seems I have to adapt the drag myself
    //ps.setDrag((if (g.nbEdges > 20000) 0.2 else 0.4).toFloat)

    val positionIndexSingle = g.position.zipWithIndex map {
      case (p, i) => (p, i, g.isSingle(i))
    }

    if (g.hashed != lastHash) {
      lastHash = g.hashed
      ps.clear
      //println("hash changed, regenerating the particle system..")

      val aMinMaxWeights = (h.minAEdgeWeight, h.maxAEdgeWeight)
      val bMinMaxWeights = (h.minBEdgeWeight, h.maxBEdgeWeight)
      val aMinMaxNodeWeights = (g.minANodeWeight, g.maxANodeWeight)
      val bMinMaxNodeWeights = (g.minBNodeWeight, g.maxBNodeWeight)

      //println("Detected min/max for Documents --> Nodes: "+aMinMaxNodeWeights+"  Links: "+aMinMaxWeights)
      //println("Detected min/max for NGrams    --> Nodes: "+bMinMaxNodeWeights+"  Links: "+bMinMaxWeights)
      //val nbEdges = g.nbEdges.toDouble / 2.0
      //val springFactor = if (nbEdges > 20000) 0.005f else 0.01f

      //val distInterval = (minLinkLength.toDouble, maxLinkLength.toDouble)

      val gravity = ps.makeParticle(GRAVITY.toFloat, 0.0f, 0.0f, 0.0f)
      gravity.makeFixed

      val positionIndexNotSingleParticle = positionIndexSingle.map {
        case ((x, y), i, s) => ((x, y), i, ps.makeParticle(1.0f, x.toFloat, y.toFloat, 0.0f))
      }

      // every node are repulsing each other (negative attraction)
      positionIndexNotSingleParticle foreach {
        case (pos1, i1, p1) =>
          positionIndexNotSingleParticle foreach {
            case (pos2, i2, p2) =>
              if (i2 != i1) {

                val strictDistance = ((g.size(i1) / 2.0) + (g.size(i2) / 2.0))
                val securityDistance = (strictDistance * 1.20) // 20%
                val distInterval = (minLinkLength.toDouble, maxLinkLength.toDouble)

                if (g.hasThisLink(i1, i2)) {

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
                    //Maths.map(w, minMaxInterval, (0.02, 0.10)).toFloat,
                    STRENGTH.toFloat,
                    DAMPING.toFloat,
                    (Maths.map(w, minMaxInterval, (0.0, 1.0)) match {
                      case l => ((1.0 - l) * (distInterval._2 - distInterval._1)) + distInterval._1
                    }).toFloat)
                }
                // no repulsion if they are not on the same date interval
                else if (!g.hasAnyLink(i1, i2)) {
                  ps.makeAttraction(p1, p2,-(if (g.connectedComponents(i1)!=g.connectedComponents(i1)) 3000 else if (pos1._1 != pos2._1) 0 else 50).toFloat, (g.size(i1) / 2.0).toFloat)
                } // default -600   we repulse unrelated nodes
              }
          }

          // ps.makeAttraction(p1, gravity, 2000f, 400f) // apply the gravity
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


            cj += 1 // okay to not start with zero here, because slot 0 is already used by gravity
            val p = ps.getParticle(cj).position()
            p.setY(Maths.limit(p.y().toDouble, -8000, 8000).toFloat)
            p.setX(nodePosition._1.toFloat)

            val (x, y) = (p.x().toDouble, p.y().toDouble)
            val v = ps.getParticle(cj).velocity()
            v.setY(Maths.limit(v.y().toDouble, -100, 100).toFloat)
            v.setX(0.toFloat)

            (x, y)
      }))
    }

  }
}
