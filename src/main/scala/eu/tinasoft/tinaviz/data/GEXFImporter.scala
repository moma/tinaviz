/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.data


import org.daizoru._
import eu.tinasoft._

import tinaviz.Maths
import tinaviz.graph._

import actors._
import Actor._

import xml._

class GEXFImporter extends node.util.Actor {
  start
  def act() {
    while (true) {
      receive {
        case rawXML:String =>
          println("GEXFImporter: parsing XML..")
          val g = new MutableGraph()
          val ns = "tina"

          val root = xml.XML.loadString(rawXML)

          var attributes : Map[String, (String,Any)] = Map.empty

          for (a <- (root \\ "attribute")) {
            attributes += (a \ "@id" text) -> ((a \ "@title" text),
                                               (a \ "@type" text) match {
                case "string" => ""
                case "float" => 0.0
                case "double" => 0.0
                case "integer" => 0.0
                case "boolean" => false
                case x => ""
              })
          }

          for (n <- (root \\ "node")) {
            val pos = n \\ "viz:position"
            var attribs : Map[String,Any] = Map.empty
            for (a <-  (n \\ "attvalue")) {
              val attr = attributes(a \ "id" text)
              val value =  (a \ "value" text)
              attribs += attr._1 -> (attr._2 match {
                case Double => value.toDouble
                case Float => value.toFloat
                case Int => value.toInt
                case x => value
                })
            }
            g.nodes ::= new MutableNode(n \ "@id" text,
                                        n \ "@label" text,
                                        try { ((pos \ "@x").text.toDouble, (pos \ "@y").text.toDouble) } catch { 
                case x => (Maths.random(0,900), Maths.random(0,500))})
          }
          for (e <- (root \\ "edge")) 
            g.node(e \ "@source" text).addNeighbour(g.id(e \ "@target" text), ((e \ "@weight").text.toDouble))

          println("GEXFImporter: loaded "+g.nbNodes+" nodes, "+g.nbEdges+" edges.")
          sender ! "graph" -> g.toGraph
          exit

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
}
