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
import java.net.URL

class GEXF extends node.util.Actor {
  start

  def act() {
    while (true) {
      receive {
        case url:URL =>  println("Loading "+url); reply(load(url))
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
    var properties = Map(
      "url" -> ""
    )
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
  
    def attribute(e:xml.Node) : (String,Any) = {
      val attr = nodeAttributes(e \ "@for" text)
      val value =  (e \ "@value" text)
      attr._1 -> (attr._2 match {
          case Double => value.toDouble
          case Float => value.toFloat
          case Int => value.toInt
          case x => value
        })
    }

    val graph = new Graph()
    var id = -1
    for (n <- (root \\ "node")) {
      id += 1

      val uuid = n \ "@id" text
      val label =  try { n \ "@label" text } catch { case x => "Node "+uuid}
      val position = try {
        (((n \\ "viz:position") \ "@x" text).toDouble,
         ((n \\ "viz:position") \ "@y" text).toDouble)
      } catch {
        case x => (Maths.random(0,200), Maths.random(0,200))
      }
      /*
       val color : Color = try {
       (((n \\ "viz:color") \ "@r" text).toInt,
       ((n \\ "viz:color") \ "@g" text).toInt,
       ((n \\ "viz:color") \ "@b" text).toInt)
       } catch {
       case x => (0,0,0)
       }*/
      val color = new Color(Maths.random(0.0,1.0),
                            Maths.random(0.8,1.0),
                            Maths.random(0.8,1.0))
      graph.set(id, "uuid" -> uuid)
      graph.set(id, "label" -> label)
      graph.set(id, "color" -> color)
      graph.set(id, "selected" -> Maths.randomBool)
      graph.set(id, "rating" -> 1)
      graph.set(id, "size" -> 1.0)
      graph.set(id, "weight" -> 1.0)
      graph.set(id, "category" -> "Default")
      graph.set(id, "position" -> position)
      graph.set(id, "position" -> position)
      graph.set(id, "linkIdArray" -> List.empty[Int])
      graph.set(id,"linkWeightArray" -> List.empty[Double])
      graph.set(id, "linkSet" -> Set.empty[Int])
      for (a <- (n \\ "attvalue")) yield graph.set(id,attribute(a))
    }

    id = -1
    for (n <- (root \\ "node")) {
      id += 1
        var links = List.empty[Int]
        var weights = List.empty[Double]
        var set = Set.empty[Int]
        for (e <- (root \\ "edge") if (n \ "@id" text).equals(e \ "@source" text)) {
          val node2id = graph.id(e \ "@target" text)
          links = links ::: List(node2id)
          set = set + node2id
          weights = weights ::: List((e \ "@weight").text.toDouble)
        }
        graph.set(id, "linkIdSet" -> set.toArray)
        graph.set(id, "linkIdArray" -> links.toArray)
        graph.set(id, "linkWeightArray" -> weights.toArray)
    }
    graph
  }

  implicit def urlToString(url:java.net.URL) : String = {
    val b = new StringBuilder; Source.fromURL(url).foreach(b.append); b.toString
  }
}
