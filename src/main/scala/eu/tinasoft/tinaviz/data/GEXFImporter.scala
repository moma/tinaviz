/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package eu.tinasoft.tinaviz.data

import org.daizoru._
import eu.tinasoft._

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
          log("parsing XML..")
          val graph = new MutableGraph()
          val ns = "tina"

          val g = xml.XML.loadString(rawXML)
        
          for (n <- (g \\ "node")) {
            val node = new tinaviz.graph.Node (
              n \ "@uuid" text,
              n \ "@label" text
            )
          }
          for (e <- (g \\ "edge")) {
            //if ((node \ "@expired").text == "true")
            //  println("the " + node.text + " has expired!")
            log("parsing edge..")
            //g.getNode(e.)
          }
          log("done parsing. sending to import controller..")
          sender ! 'gexfImport -> graph.toGraph
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
           </gexf>*/

          sender ! 'gexfExport -> xml
          exit
      }
    }
  }

}
