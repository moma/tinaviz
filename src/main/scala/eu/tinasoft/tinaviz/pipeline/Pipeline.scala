/************************************************************************
                                  Tinaviz
*************************************************************************
 This application is part of the Tinasoft project: http://tinasoft.eu
 Tinaviz main developer: julian.bilcke @ iscpif.fr  (twitter.com/flngr)

 Copyright (C) 2009-2011 CREA Lab, CNRS/Ecole Polytechnique UMR 7656 (Fr)

 This program is free software: you can redistribute it and/or modify it
 under the terms of the GNU General Public License as published by the
 Free Software Foundation, either version 3 of the License, or (at your
 option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program. If not, see <http://www.gnu.org/licenses/>.
************************************************************************/

package eu.tinasoft.tinaviz.pipeline

import eu.tinasoft._
import tinaviz.graph._

import java.util.concurrent.atomic.AtomicReference

/**
 * Grameda - Graph Memory Database
 * At first was a standard Scala Actor, but _fast_ wasn't __FAST__ enough
 */
object Pipeline {

  val _input = new AtomicReference(new Graph)
  def setInput(g:Graph) { _input.set(g) }
  def input : Graph = _input.get

  val _categoryCache = new AtomicReference(new Graph)
  def setCategoryCache(g:Graph) { _categoryCache.set(g) }
  def categoryCache : Graph = _categoryCache.get

  val _nodeWeightCache = new AtomicReference(new Graph)
  def setNodeWeightCache(g:Graph) { _nodeWeightCache.set(g) }
  def nodeWeightCache : Graph = _nodeWeightCache.get

  val _edgeWeightCache = new AtomicReference(new Graph)
  def setEdgeWeightCache(g:Graph) { _edgeWeightCache.set(g) }
  def edgeWeightCache : Graph = _edgeWeightCache.get

  val _output = new AtomicReference(new Graph)
  def setOutput(g:Graph) { _output.set(g) }
  def output : Graph = _output.get

  def applyKey(key:String, value:Any)= {
    //println("updating graph attribute " + key + " -> " + value)
    setInput(input + (key -> value))
    setCategoryCache(categoryCache + (key -> value))
    setNodeWeightCache(nodeWeightCache + (key -> value))
    setEdgeWeightCache(edgeWeightCache + (key -> value))
    setOutput(output + (key -> value))
  }
}
