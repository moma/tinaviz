package eu.tinasoft.tinaviz

import actors.Actor
import io.Webpage
import layout.Layout
import pipeline.{Workflow, Pipeline}
import java.applet.Applet

class Session (val applet:Applet) {

  val webpage = new Webpage (this)
  val workflow = new Workflow (this)
  val server = new Server (this)
  val layout = new Layout (this)

  val pipeline = new Pipeline

  val services : List[Actor] = List(webpage, workflow, server, layout)

  def start = services.foreach( _.start )
  def close = {
    services.foreach( _ ! 'exit )
    Thread.sleep(2000)
  }

}