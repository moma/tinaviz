/************************************************************************
                                  Tinaviz
*************************************************************************
 This application is part of the Tinasoft project: http://tinasoft.eu
 Tinaviz main developer: julian.bilcke @ iscpif.fr  (twitter.com/flngr)

Copyright (C) 2009-2011 CREA Lab, CNRS/Ecole Polytechnique UMR 7656 (Fr)
************************************************************************/

package eu.tinasoft.tinaviz.io

import eu.tinasoft._

import tinaviz.util.Color
import tinaviz.util.Color._
import tinaviz.util.Rio
import tinaviz.util.Maths

import tinaviz.graph._

import actors._
import Actor._

import io.Source

import xml._
import java.net.{URLConnection, URL, Authenticator, PasswordAuthentication}
//import java.io.{FileInputStream,FileOutputStream,IOException}
//import java.util.zip.InflaterInputStream
import java.util.zip.GZIPInputStream
import java.io.BufferedInputStream

class GEXF extends Actor {

   start

  def act() {

      receive {

        case 'exit =>
          println("GEXF: exiting..")
          exit()

        case url: URL =>
            println("Connecting to " + url)
            val BUFFER_SIZE = 2048
            val conn = url.openConnection
            val ins = conn.getInputStream

            reply(
              load(
              if (url.toString.endsWith(".gexf")||url.toString.endsWith(".xml")) {
                 println("Reading raw graph stream, please wait..")
                 XML.load(ins)
              } else if (url.toString.endsWith(".zip")||url.toString.endsWith(".gz")||url.toString.endsWith(".tar.gz")) {
               println("Reading gzipped graph stream, please wait..")
                val ins2 = new BufferedInputStream(  // TO BE CLOSE
                    new GZIPInputStream(
                      ins,
                      BUFFER_SIZE
                    ),
                    BUFFER_SIZE
                )
                try XML.load(ins2) finally ins2.close

              } else {
                XML.load(ins)
              }
            )
          )

        case str: String =>
            println("Reading graph string, please wait..")
            reply(load(XML.load(str)))


        case graph: Graph =>
          val newColors = graph.renderNodeColor map {
            case (col) => col.toRGBTuple3
          }
          reply (
            <gexf xmlns="http://www.gexf.net/1.1draft" xmlns:viz="http://www.gexf.net/1.1draft/viz.xsd">
              <meta lastmodifieddate="1986-03-24">
                <creator>tinaviz2</creator>
                <description>Graph export</description>
              </meta>
              <graph type="static">
                <attributes class="node" type="static">
                  <attribute id="0" title="category" type="string"/>
                </attributes>
                <nodes>{
                   for ((nodeUUID,nodeIndex) <- graph.uuid.zipWithIndex) yield
                     <node id={nodeUUID} label={ graph.label(nodeIndex) }>
                       { val (x,y) = graph.position(nodeIndex) match { case (x,y) => (x.toString, y.toString)  }
                        <viz:position x={ x } y={ y } z="0.0" /> }
                       { val (r,g,b) = newColors(nodeIndex) match { case (r,g,b) => (r.toInt.toString, g.toInt.toString, b.toInt.toString ) }
                        <viz:color r={ r } g={ g } b={ b } /> }
                        <viz:size value={ graph.size(nodeIndex).toString } />
                        <attvalues>
                          <attvalue id="0" value={ graph.category(nodeIndex) }/>
                        </attvalues>
                      </node>
                  }</nodes>
                <edges>{
                   var edgeIndex = 0
                   for ((links,nodeIndex) <- graph.links.zipWithIndex; (target, weight) <- links) yield {
                     edgeIndex += 1
                     //if (graph.hasThisLink(target,nodeIndex)) {
                         //<edge id={ nodeIndex.toString } source={ graph.uuid(nodeIndex) } target={ graph.uuid(target) } type="undirected" weight={ weight.toString }> </edge>
                     // } else {
                     //if (nodeIndex < target) {
                         <edge id={ edgeIndex.toString } source={ graph.uuid(nodeIndex) } target={ graph.uuid(target) } type="undirected" weight={ weight.toString }> </edge>
                     //}
                    }
                  }</edges>
              </graph>
            </gexf>
          )
      }
  }


  def load(root:Elem) = {
    var properties = Map(
      "url" -> ""
    )
    var nodeAttributes = Map[String, (String, Any)]()
    var edgeAttributes = Map[String, (String, Any)]()
    for (as <- (root \\ "attributes")) {
      (as \\ "@class" text) match {
        case "node" =>
          for (a <- (as \\ "attribute")) {
            nodeAttributes += (a \ "@id" text) -> ((a \ "@title" text),
                                                   (a \ "@type" text) match {
                case "float" => 1.0
                case "double" => 1.0
                case "integer" => 1
                case "boolean" => false
                case "string" => ""
                case x => ""
              })
          }
        case "edge" =>
          for (a <- (as \\ "attribute")) {
            edgeAttributes += (a \ "@id" text) -> ((a \ "@title" text),
                                                   (a \ "@type" text) match {
                case "float" => 1.0
                case "double" => 1.0
                case "integer" => 1
                case "boolean" => false
                case "string" => ""
                case x => ""
              })
          }
      }
    }

    def title(s: String) = { s split(" ") map(_.capitalize) mkString(" ")  }


    def attribute(e: xml.Node): (String, Any) = {
      val attr = nodeAttributes(e \ "@for" text)
      val value = (e \ "@value" text)
      (attr._1, attr._2 match {
          case x: Double => value.toDouble
          case x: Float => value.toDouble
          case x: Int => value.toInt
          case x: Boolean => value.toBoolean
          case x: String => value.toString
          case x => value
        })

    }
    var g = new Graph()
    var id = -1
    for (n <- (root \\ "node")) {
      id += 1

      val uuid = n \ "@id" text
      val label = try {
        n \ "@label" text
      } catch {
        case x => "Node " + uuid
      }


         val p = (n \\ "position")
         //println("x: " +(p \ "@x" text)+" y:"+(p \ "@y" text))

         val position = ((try { (p \ "@x" text) match { case "" => println("bad X for "+label+"") ; 0.0 case any => any.toDouble } } catch { case e => Maths.random(0, 200) }) * 100.0,
                         try { (p \ "@y" text) match { case "" => println("bad Y for "+label+"") ; 0.0 case any => any.toDouble } } catch { case e => Maths.random(0, 200) })



      /*
       val color : Color = try {
       (((n \\ "color") \ "@r" text).toInt,
       ((n \\ "color") \ "@g" text).toInt,
       ((n \\ "color") \ "@b" text).toInt)
       } catch {
       case x => (0,0,0)
       }*/
      val color = new Color(Maths.random(0.0, 1.0),
                            Maths.random(0.8, 1.0),
                            Maths.random(0.8, 1.0))
      g += (id, "uuid", uuid)
      g += (id, "label", title(label))
      g += (id, "color", color)
      g += (id, "selected", false)
      g += (id, "highlighted", false)
      g += (id, "updateStatus", 'outdated ) // outdated, updating, updated, failure
      g += (id, "saveSatatus", 'saved ) // saving, saved

      g += (id, "density", 1.0)
      g += (id, "rate", 1)
      g += (id, "size", 1.0)
      g += (id, "weight", 1.0)
      g += (id, "category", "Document")
      g += (id, "content", " ")
      g += (id, "position", position)
      g += (id, "links", Map.empty[Int, Double])

      var keywords = List.empty[String]
      for (a <- (n \\ "attvalue")) {

          val at = attribute(a)
             if (at._1.equalsIgnoreCase("keyword")) {
                 keywords ::= (at._2 match { case s:String => s case a => ""})
             } else {
                 g += (id,at._1, at._2)
            }
      }
      g += (id, "content", g.content(id).replaceAll("\"","&quot;").replaceAll("'", "&#39;"))
      //println("added size "+g.getArray[Double]("weight")(id))
      g += (id, "keywords", keywords.toList.toArray)
    }



    for (e <- (root \\ "edge")) {
      val node1uuid = e \ "@source" text
      val node2uuid = e \ "@target" text
      val weight = ((e \ "@weight" text) match {
          case "" => "0"
          case s:String => s
          case any => "0"
      }).toDouble

      val undirected = try {
             (e \ "@type" text) match {
                  case "undirected" => true
                  case "directed" => false
                  case any => false
            }
      } catch {
         case e => false
      }

      val lnks = g.getArray[Map[Int, Double]]("links")

      if (!node1uuid.equals(node2uuid)) {
        val node1id = g.id(node1uuid)
        val node2id = g.id(node2uuid)
        g += (node1id, "links", lnks(node1id) + (node2id -> weight))
        if (undirected)
           g += (node2id, "links", lnks(node2id) + (node1id -> weight))
      }
    }

    val h = Graph.make(g.elements)//.normalizePositions
    val (centerX, centerY) = Metrics.basicCenter(h)
    println("center is: "+(centerX,centerY))
    h + ("position" -> (h.position map { case (x,y) => (x - centerX, y - centerY) }))
  }


  /*
   implicit def urlToString(url: java.net.URL): String = {
   val b = new StringBuilder
   Source.fromURL(url).foreach(b.append)
   b.toString
   }*/
}
