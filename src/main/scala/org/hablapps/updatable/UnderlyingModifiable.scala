/*
 * Copyright (c) 2013 Habla Computing
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.hablapps.updatable

import scala.language.higherKinds
import scala.language.postfixOps

/** Knows how to modify a collection. */
trait UnderlyingModifiable {

  /** The type of the collection for which this class is valid. */
  type Col[_]

  def empty[E]: Col[E]

  /** Returns the modificated collection, where `e` would had been added or
    * removed, depending on the `add` flag. 
    *
    * @tparam E the element type
    * @param c the collection to modify
    * @param e the modification to achieve
    * @param add the mode of the modification (true to add, false to remove)
    */
  def modify[E](c: Col[E], e: E, add: Boolean): Col[E]

  /** Returns the resulting collection of applying filter `f` to the original
    * `c`.
    * 
    * @tparam E the element type
    * @param c the collection to filter
    * @param f the filter to apply
    */
  def filter[E](c: Col[E])(f: E => Boolean): Col[E]

  /** Turns collection `c` into a list.
    *
    * @tparam E the element type
    * @param c the collection to convert
    */
  def toList[E](c: Col[E]): List[E]
}

/** Implicit modifiables for the most common cases.
  *
  * You can find an example of an `Option` modifiable below.
  * {{{
  * scala> import org.hablapps.updatable.UnderlyingModifiable._
  * import org.hablapps.updatable.UnderlyingModifiable._
  *
  * scala> val opt = Option(5)
  * opt: Option[Int] = Some(5)
  *
  * scala> modifiable(opt).modify(opt, 6, true)
  * res0: Option[Int] = Some(6)
  *
  * scala> modifiable(opt).modify(opt, 5, false)
  * res1: Option[Int] = None
  * }}}
  */
object UnderlyingModifiable {

  def modifiable[C[_], E](arg: C[E])(implicit e: Modifiable[C]): Modifiable[C] = 
    implicitly[Modifiable[C]]

  /** Returns a modifiable for options. */
  implicit val option: Modifiable[Option] = new UnderlyingModifiable {
    type Col[X] = Option[X]

    def empty[E]: Option[E] = None

    def modify[E](c: Option[E], e: E, add: Boolean): Option[E] =
      if (add)
        Option(e)
      else if (c.isDefined && c.get == e)
        None
      else
        c

    def filter[E](c: Option[E])(f: E => Boolean): Option[E] = c filter f

    def toList[E](c: Option[E]): List[E] = c.toList

    override def toString = "Option Modifiable"
  }

  import scala.collection.{ generic, TraversableLike, TraversableOnce }
  import generic.GenericTraversableTemplate

  /** Returns a modifiable for traversables. */
  implicit def traversable[C[X] <: Traversable[X] 
      with TraversableLike[X, C[X]] 
      with GenericTraversableTemplate[X, C] 
      with TraversableOnce[X]]: Modifiable[C] = new UnderlyingModifiable {
    type Col[X] = C[X]

    def empty[E]: Col[E] = higherDefault[C, E]

    def modify[E](c: Col[E], e: E, add: Boolean): Col[E] =
      if (add)
        c.genericBuilder[E] ++= c += e result
      else
        c filter { _ != e }

    def filter[E](c: Col[E])(f: E => Boolean): Col[E] = c filter f
      
    def toList[E](c: Col[E]): List[E] = c.toList

    override def toString = "Traversable Modifiable"
  }
}
