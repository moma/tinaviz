package eu.tinasoft.tinaviz.util

/**
 * Created by IntelliJ IDEA.
 * User: jbilcke
 * Date: 6/22/11
 * Time: 12:58 PM
 * To change this template use File | Settings | File Templates.
 */

object Lists {

  /**
   * http://langref.org/scala/lists/manipulation/list-gather
   */
  def zip3(l1: List[_], l2: List[_], l3: List[_]): List[Tuple3[_, _, _]] = {
    def zip3$(l1$ : List[_], l2$ : List[_], l3$ : List[_], acc: List[Tuple3[_, _, _]]): List[Tuple3[_, _, _]] = l1$ match {
      case Nil => acc reverse
      case l1$head :: l1$tail => zip3$(l1$tail, l2$.tail, l3$.tail, Tuple3(l1$head, l2$.head, l3$.head) :: acc)
    }

    zip3$(l1, l2, l3, List[Tuple3[_, _, _]]())
  }

    def zip4(l1: List[_], l2: List[_], l3: List[_], l4: List[_]): List[Tuple4[_, _, _, _]] = {
    def zip4$(l1$ : List[_], l2$ : List[_], l3$ : List[_], l4$ : List[_], acc: List[Tuple4[_, _, _, _]]): List[Tuple4[_, _, _, _]] = l1$ match {
      case Nil => acc reverse
      case l1$head :: l1$tail => zip4$(l1$tail, l2$.tail, l3$.tail, l4$.tail, Tuple4(l1$head, l2$.head, l3$.head, l4$.head) :: acc)
    }

    zip4$(l1, l2, l3, l4, List[Tuple4[_, _, _, _]]())
  }


}