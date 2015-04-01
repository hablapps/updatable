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

import scala.reflect.runtime.universe.TypeTag
import language.higherKinds

/** Does contain a default value for a type.
  *
  * This is intended to be the parent trait for a big range of default values,
  * whatever the type. The ''updatable'' project uses this as a type class to
  * offer initial values while instantiating the entities.
  *
  * @tparam A the type that requires a default value
  * @see [[org.hablapps.updatable.Builder]]
  */
trait Default[A] {

  /** Returns the default value for the type A. */
  def value: A
}

/** Implicit defaults for the most common scala types.
  *
  * They can be used the following way:
  * {{{
  * val a: Int = implicitly[Default[Int]].value
  * }}}
  * It is obvious that this usage is quite verbose. As a result, to make it
  * easier, ''updatable'' offers a '''default[A]''' which finds the implicit
  * default and extracts the value:
  * {{{
  * val a: Int = default[Int]
  * }}}
  */
object Default {

  /** Returns a default for `Boolean`. */
  implicit def booleanDefault: Default[Boolean] =
    new Default[Boolean] {
      def value = false
    }

  /** Returns a default for `Int`. */
  implicit def intDefault: Default[Int] =
    new Default[Int] {
      def value = 0
    }

  /** Returns a default for `Double` */
  implicit def doubleDefault: Default[Double] =
    new Default[Double] {
      def value = 0.0
    }

  /** Returns a default for `Long` */
  implicit def longDefault: Default[Long] =
    new Default[Long] {
      def value = 0
    }

  /** Returns a default for `Float` */
  implicit def floatDefault: Default[Float] =
    new Default[Float] {
      def value = 0
    }

  /** Returns a default for `String` */
  implicit def stringDefault: Default[String] =
    new Default[String] {
      def value = ""
    }

  /** Returns a default for `Char` */
  implicit def charDefault: Default[Char] =
    new Default[Char] {
      def value = '\u0000'
    }

  implicit def optionDefault[T]: Default[Option[T]] =
    new Default[Option[T]] {
      def value = None
    }

  implicit def someDefault[T: Default: TypeTag]: Default[Some[T]] =
    new Default[Some[T]] {
      def value = Some(default[T])
    }

  /** Returns a default for `List`.
    *
    * @tparam A the List's type parameter
    */
  implicit def listDefault[A]: Default[List[A]] =
    new Default[List[A]] {
      def value = List()
    }

  /** Returns a default for `Traversable`.
    *
    * @tparam A the Traversable's type parameter
    */
  implicit def traversableDefault[A]: Default[Traversable[A]] =
    new Default[Traversable[A]] {
      def value = Traversable()
    }

  /** Returns a default for `Set`.
    *
    * @tparam A the Set's type parameter
    */
  implicit def setDefault[A]: Default[Set[A]] =
    new Default[Set[A]] {
      def value = Set()
    }
}

trait HigherDefault[C[_]]{ 
  def value[T]: C[T]
}


object HigherDefault{ 

  implicit def optionDefault = new HigherDefault[Option]{ 
   def value[T]: Option[T] = None
  }

  implicit def setDefault = new HigherDefault[Set]{ 
   def value[T]: Set[T] = Set()
  }

  implicit def listDefault = new HigherDefault[List]{ 
   def value[T]: List[T] = List()
  }

  implicit def traversableDefault = new HigherDefault[Traversable]{ 
   def value[T]: Traversable[T] = Traversable()
  }

}
