/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.data

import org.daizoru._
import eu.tinasoft._

import tinaviz.Color
import Color._

import tinaviz.Maths

import tinaviz.graph._
import MutableGraph._

import actors._
import Actor._

import io.Source

import xml._
import java.net.URL

class GEXFImporter extends node.util.Actor {
  start

  def act() {
    while (true) {
      receive {
        case url:URL => reply(load(url))
        case xml:String => reply(load(xml))

        case graph:Graph =>
          val xml = ""
          /*
           <gexf xmlns="http://www.gexf.net/1.1draft" xmlns:viz="http://www.gexf.net/1.1draft/viz.xsd">
           <graph type="static">
           <attributes class="node" type="dynamic">
           <attribute id="0" title="category" type="string"/>
           <attribute id="1" title="proximity" type="float"/>
           </attributes>

           <!-- session parameters (saved within the URL) -->
           <tina>
           </tina>
           <nodes>
           {for (node <- graph.nodes) yield
           <node id="graph.getId(node)" label={node.label}>
           <!--<viz:color b={node.color._1} g={node.color._2} r={node.color._3}/>-->
           <viz:position x={node.position._1} y={node.position._2} z="0.0"/>
           <viz:size value={node.size}/>
           <attvalues>
           <attvalue id="0" value="project"/>
           <attvalue id="1" value="0.1"/>
           </attvalues>
           </node>
           }
           </nodes>
           <edges>
           <edge></edge>
           </edges>
           </graph>
           </gexf>
           */
          sender ! 'gexfExport -> xml
          exit
      }
    }
  }


  def load (rawXML:String) = {
    val root = xml.XML.loadString(rawXML)
    var nodeAttributes = Map[String, (String,Any)]()
    var edgeAttributes = Map[String, (String,Any)]()
    for (as <- (root \\ "attributes")) {
      (as \\ "@class" text) match {
        case "node" =>
          for (a <- (as \\ "attribute")) {
            nodeAttributes += (a \ "@id" text) -> ((a \ "@title" text),
                                                   (a \ "@type" text) match {
                case "float" => 1f
                case "double" => 1.0
                case "integer" => 1
                case "boolean" => false
                case x => ""
              })
          }
        case "edge" =>
          for (a <- (as \\ "attribute")) {
            edgeAttributes += (a \ "@id" text) -> ((a \ "@title" text),
                                                   (a \ "@type" text) match {
                case "float" => 1f
                case "double" => 1.0
                case "integer" => 1
                case "boolean" => false
                case x => ""
              })
          }
      }
    }

    def attribute(e:xml.Node) = {
      val attr = nodeAttributes(e \ "@for" text)
      val value =  (e \ "@value" text)
      attr._1 -> (attr._2 match {
          case Double => value.toDouble
          case Float => value.toFloat
          case Int => value.toInt
          case x => value
        })
    }
    val g = new MutableGraph()
    for (n <- (root \\ "node")) {
      var attributes = Map[String,Any]()
      val uuid = n \ "@id" text
      val label =  try {
        n \ "@label" text
      } catch {
        case x => "Node "+uuid
      }

      val position = try {
        (((n \\ "viz:position") \ "@x" text).toDouble,
         ((n \\ "viz:position") \ "@y" text).toDouble)
      } catch {
        case x => (Maths.random(0,900), Maths.random(0,500))
      }

      val color : Color = try {
        (((n \\ "viz:color") \ "@r" text).toInt,
         ((n \\ "viz:color") \ "@g" text).toInt,
         ((n \\ "viz:color") \ "@b" text).toInt)
      } catch {
        case x => (0,0,0)
      }

      attributes ++= Map (
        "uuid" -> uuid,
        "label" -> label,
        "color" -> color,
        "category" -> "default",
        "weight" -> 1.0
      ) ++ (for (a <- (n \\ "attvalue")) yield attribute(a))

      g.nodes ::= new MutableNode(uuid,
                                  label,
                                  position,
                                  color,
                                  attributes)
    }
    for (e <- (root \\ "edge"))
      g.node(e \ "@source" text).addNeighbour(g.id(e \ "@target" text),
                                              ((e \ "@weight").text.toDouble))

    g:Graph
  }

  implicit def urlToString(url:java.net.URL) : String = {
    val b = new StringBuilder; Source.fromURL(url).foreach(b.append); b.toString
  }
}
