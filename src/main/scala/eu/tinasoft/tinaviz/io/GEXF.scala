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
  
    def attribute(id:Int,e:xml.Node) : (Int,String,Any) = {
      val attr = nodeAttributes(e \ "@for" text)
      val value =  (e \ "@value" text)
      val r = (id,attr._1, attr._2 match {
          case Double => value.toDouble
          case Float => value.toFloat
          case Int => value.toInt
          case x => value
        })

      println(" found ttribute: "+r)
      r
    }

    var g = new Graph()
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
      g += (id, "uuid", uuid)
      g += (id, "label", label)
      g += (id, "color", color)
      g += (id, "selected", Maths.randomBool)
      g += (id, "rating", 1)
      g += (id, "size", 1.0)
      g += (id, "weight", 1.0)
      g += (id, "category", "Default")
      g += (id, "position", position)
      g += (id, "linkIdArray", List.empty[Int])
      g += (id,"linkWeightArray", List.empty[Double])
      g += (id, "linkSet", Set.empty[Int])
       println("I")
      //for (a <- (n \\ "attvalue")) yield { g += attribute(id,a) }
       println("J")
    }

    id = -1
    for (n <- (root \\ "node")) {
      id += 1
        var links = List.empty[Int]
        var weights = List.empty[Double]
        var set = Set.empty[Int]
        for (e <- (root \\ "edge") if (n \ "@id" text).equals(e \ "@source" text)) {
          val node2id = g.id(e \ "@target" text)
          links = links ::: List(node2id)
          set = set + node2id
          weights = weights ::: List((e \ "@weight").text.toDouble)
        }
        g += (id, "linkIdSet",set.toArray)
        g += (id, "linkIdArray", links.toArray)
        g += (id, "linkWeightArray", weights.toArray)
    }
    g.computeAll
  }

  implicit def urlToString(url:java.net.URL) : String = {
    val b = new StringBuilder; Source.fromURL(url).foreach(b.append); b.toString
  }
}
