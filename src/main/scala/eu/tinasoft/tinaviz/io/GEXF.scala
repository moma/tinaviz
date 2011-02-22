/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.io

import org.daizoru._

import eu.tinasoft._
import tinaviz.util._
import Color._

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
          reply(XML.load(conn.getInputStream))
        }
        case str: String => {
          println("Reading graph string, please wait..")
          reply(XML.load(str))
        }

        case graph: Graph => reply (
           <gexf xmlns="http://www.gexf.net/1.1draft" xmlns:viz="http://www.gexf.net/1.1draft/viz.xsd">
              <graph type="static">
                 <attributes class="node" type="dynamic">
                 <attribute id="0" title="category" type="string"/>
                 </attributes>
              <nodes>{ 
               for ((nodeUUID,nodeIndex) <- graph.uuid.zipWithIndex) yield 
                 <node id={nodeUUID} label={ graph.label(nodeIndex) }>
                   { val p = graph.position(nodeIndex) }
                   <!--<viz:color b={node.color._1} g={node.color._2} r={node.color._3}/>-->
                   <!--<viz:position x={graph.position(i)._1} y={graph.position(i)._2} z="0.0"/>-->
                   <!--<viz:size value={graph.size(i)}/>-->
                   <attvalues>
                     <attvalue id="0" value={ graph.category(nodeIndex) }/>
                   </attvalues>
                 </node>
              }</nodes>
              <edges>{ 
               var edgeIndex = 0
               for ((links,nodeIndex) <- graph.links.zipWithIndex; (target, weight) <- links) yield {
                 edgeIndex += 1
                 <edge id={ nodeIndex.toString } source={ graph.uuid(nodeIndex) } target={ graph.uuid(target) } weight={ weight.toString }>
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
      g += (id, "label", label)
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

      for (a <- (n \\ "attvalue")) g += (id, attribute(a)._1, attribute(a)._2)
    }

    for (e <- (root \\ "edge")) {
      val node1uuid = e \ "@source" text
      val node2uuid = e \ "@target" text

      if (!node1uuid.equals(node2uuid)) {
        val node1id = g.id(node1uuid)
        val node2id = g.id(node2uuid)
        g += (node1id, "links", g.getArray[Map[Int, Double]]("links")(node1id) + (node2id -> (e \ "@weight").text.toDouble))
      }
    }
    Graph.make(g.elements)
  }

  
/*
  implicit def urlToString(url: java.net.URL): String = {
    val b = new StringBuilder
    Source.fromURL(url).foreach(b.append)
    b.toString
  }*/
}
