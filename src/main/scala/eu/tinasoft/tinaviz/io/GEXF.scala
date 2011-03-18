/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.io

import org.daizoru._

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

class GEXF extends node.util.Actor {
  start

  def act() {
    while (true) {
      receive {
        case url: URL => {
            println("Connecting to " + url)
            val conn = url.openConnection
            println("Reading graph stream, please wait..")
            reply(load(XML.load(conn.getInputStream)))
          }
        case str: String => {
            println("Reading graph string, please wait..")
            reply(load(XML.load(str)))
          }

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
                       { val p = graph.position(nodeIndex)
                        <viz:position x={ p._1.toString } y={ p._2.toString } z="0.0"/> }
                       { val (r,g,b) = newColors(nodeIndex)
                        <viz:color b={ r.toInt.toString } g={ g.toInt.toString } r={ b.toInt.toString }/> }
                        <viz:size value={ graph.size(nodeIndex).toString }/>
                        <attvalues>
                          <attvalue id="0" value={ graph.category(nodeIndex) }/>
                        </attvalues>
                      </node>
                  }</nodes>
                <edges>{
                   var edgeIndex = 0
                   for ((links,nodeIndex) <- graph.links.zipWithIndex; (target, weight) <- links) yield {
                     edgeIndex += 1
                     <edge id={ nodeIndex.toString } source={ graph.uuid(nodeIndex) } target={ graph.uuid(target) } type="undirected" weight={ weight.toString }>
                     </edge>
                    }
                  }</edges>
              </graph>
            </gexf>
          )
      }
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
      val position = try {
        (((n \\ "viz:position") \ "@x" text).toDouble,
         ((n \\ "viz:position") \ "@y" text).toDouble)
      } catch {
        case x => (Maths.random(0, 200), Maths.random(0, 200))
      }
      /*
       val color : Color = try {
       (((n \\ "viz:color") \ "@r" text).toInt,
       ((n \\ "viz:color") \ "@g" text).toInt,
       ((n \\ "viz:color") \ "@b" text).toInt)
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
      val weight =  (e \ "@weight" text).toDouble
      val undirected = (e \ "@type" text) match {
        case "undirected" => true
        case "directed" => false
        case any => false
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

    Graph.make(g.elements)//.normalizePositions
  }


  /*
   implicit def urlToString(url: java.net.URL): String = {
   val b = new StringBuilder
   Source.fromURL(url).foreach(b.append)
   b.toString
   }*/
}
