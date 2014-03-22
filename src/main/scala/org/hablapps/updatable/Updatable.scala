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
import scala.reflect.{classTag,ClassTag}

/** Does wrap a builder and an entity value. */
abstract class Updatable[+S] {

  /** The entity type. */
  type Tpe <: S

  /** The wrapped builder. */
  implicit val builder: Builder[Tpe]

  /** The wrapped entity value. */
  val value: Tpe

  /**
  * Tests the type wrapped by this updatable value
  */
  def isA[T: ClassTag] = classTag[T].runtimeClass.isAssignableFrom(builder._class.runtimeClass)

  /** Returns the value of attribute `a` from current `value`.
    *
    * @param a the attribute whose value the user is interested in
    */
  def get(a: model.Attribute): Any = builder.get(value, a)

  /** Returns the value of an attribute named `a` from current `value`.
    *
    * @param a the name of the attribute whose value the user is interested in
    */
  def get(a: String): Any = builder.get(value, toReification(a))

  /** Returns the extension of attribute `a` from current `value`.
    *
    * @param a the attribute to retrieve the extension
    */
  def extension(a: model.Attribute): List[Any] = builder.extension(value, a)

  /** Returns the extension of attribute `a` from current `value`.
    *
    * @tparam A the type of extension the user is expecting to receive
    * @param a the attribute to get the extension
    */
  def extensionT[A](a: model.Attribute): List[A] =
    builder.extension(value, a).asInstanceOf[List[A]]

  /** Returns a new `Updatable` whose `value` has been updated regarding the
    * original value. The update has consisted on updating attribute `a` with
    * the value `v`.
    *
    * @param a the attribute to update
    * @param v the value to set to `a`
    */
  def updated(a: model.Attribute, v: Any): Updatable[S] =
    builder.updated(value, a, v)

  /** Returns a new `Updatable` whose `value` has been updated regarding the
    * original value. The update has consisted on updating an attribute named
    * `a` with the value `v`.
    *
    * @param a the name of the attribute to update
    * @param v the value to set to `a`
    */
  def updated(a: String, v: Any): Updatable[S] = try {
    updated(toReification(a), v)
  } catch { case e: Throwable => 
    System.err.println(s"Can't update attribute $a with value $v")
    throw e
  }

  /** Returns a new `Updatable` whose `value` has been modified regarding the
    * original value. The modification has consisted on modify the attribute
    * `a` by adding or removing (`mode`) the value `v`.
    *
    * @param a the attribute to modify
    * @param v the modification value
    * @param mode true and false represent positive and negative modification,
    * respectively
    */
  def modify(a: model.Attribute, v: Any, mode: Boolean): Updatable[S] =
    builder.modify(value, a, v, mode)

  /** Returns a new `Updatable` whose `value` has been modified regarding the
    * original value. The modification has consisted on modify an attribute
    * named `a` by adding or removing (`mode`) the value `v`.
    *
    * @param a the name of the attribute to modify
    * @param v the modification value
    * @param mode true and false represent positive and negative modification,
    * respectively
    */
  def modify(a: String, v: Any, mode: Boolean): Updatable[S] =
    modify(toReification(a), v, mode)

  /** This is simply an alias for a positive `modify`. */
  def ++=(a: model.Attribute, v: Any): Updatable[S] = modify(a, v, true)

  /** This is simply an alias for a negative `modify`. */
  def --=(a: model.Attribute, v: Any): Updatable[S] = modify(a, v, false)

  /** Returns the list of attribute reifications. */
  val attributes: List[model.Attribute] = builder.attributes

  /** Returns the attribute reification named `attname`.
    *
    * @param attname the name of the attribute
    */
  def toReification(attname: String): model.Attribute =
    builder.toReification(attname)

  override def equals(other: Any): Boolean = other match {
    case that: Updatable[_] => value == that.value
    case _ => false
  }

  override def toString(): String = builder.show(value)
}

/** Factory for [[org.hablapps.updatable.Updatable]] instances. */
object Updatable {
  def apply[A](t: A)(implicit e: Builder[A]): Updatable[A] = toUpdatable(t)
  def unapply[A](t: Updatable[A]): Option[(A)] = Some(t.value)
}
