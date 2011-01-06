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

class ImportController extends node.util.Actor {

  start

  def act() {

    loop {
      react {

        // public messages
        case ('fromURI,uri:String) =>
          // launch a temporary threaded GEXFImporer, that will die
          val importer : Actor = new GEXFImporter()
          importer ! """
          <sammich>
            <bread>wheat</bread>
            <meat>salami</meat>
            <condiments>
              <condiment expired="true">mayo</condiment>
              <condiment expired="false">mustard</condiment>
            </condiments>
          </sammich>"""

        case ('fromString,graph:String) =>
          // launch a temporary threaded GEXFImporer, that will die
          val importer : Actor = new GEXFImporter()
          importer ! """
          <sammich>
          <bread>wheat</bread>
          <meat>salami</meat>
          <condiments>
            <condiment expired="true">mayo</condiment>
            <condiment expired="false">mustard</condiment>
          </condiments>
          </sammich>
          """

        // used internally
        case('gexfExport,gexf:String) =>
          log("successfully exported graph: "+gexf)
          
        case ('gexfImport,graph:Graph) =>
          log("successfully imported graph: "+graph)
                    
        case msg => log("unknow msg: "+msg)
      }
    }

  }
}
