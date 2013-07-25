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
import scala.collection.GenTraversable
import scala.reflect.{ ClassTag, classTag }
import scala.reflect.runtime.universe.{ typeTag, TypeTag, typeOf }
import java.util.NoSuchElementException

/** Does contain the basic building information of an entity.
  *
  * This is the reduced version of [[org.hablapps.updatable.Builder]] and should
  * be used when dealing with abstract entities, which cannot be updated nor
  * instantiated. The major duty of this weak builder is to keep the attribute
  * reifications of the attributes '''declared''' by the entity.
  *
  * Builders instantiate entities, update them and keep metainformation about
  * the entity itself, among many other things. Regarding metainformation, one
  * of the most important elements are the attribute reifications. An attribute
  * reification references an attribute, and it is a basic pillar to create or
  * update an entity instance.
  *
  * It is quite common to find an entity extending another one, conforming a
  * hierarchy. Just to avoid duplicating work, the builders associated with the
  * parent entities must share their attribute reifications with the builders of
  * the child entities. As a result, if an entity does not have an associated
  * builder, the children entities cannot make a builder for themselves. So,
  * attribute reifications should be always available. Because of that, while
  * dealing with abstract entities, which cannot be instantiated but can declare
  * new attributes, a ''weak'' version is required.
  *
  * It may worth noting that it is feasible to create this kind of builder for a
  * ''concrete'' entity, but totally discouraged, since it could decrease the
  * readability of the final code. The reason why that happens is because this
  * kind of builder is generated with a macro ''weakBuilder[A]''. If the reader
  * sees this kind of invocation attached to a concrete type, he could infer
  * wrongly that the type is abstract instead.
  *
  * {{{
  * scala> import org.hablapps.updatable._
  * import org.hablapps.updatable._
  *
  * scala> :paste
  * // Entering paste mode (ctrl-D to finish)
  *
  * trait A {
  *   type A1
  *
  *   val att1: A1
  *   val att2: String
  * }
  * val A = weakBuilder[A]
  *
  * // Exiting paste mode, now interpreting.
  *
  * defined trait A
  * ...
  *
  * scala> import language.reflectiveCalls
  * import language.reflectiveCalls
  *
  * scala> A._att1
  * res0: org.hablapps.updatable.model.Attribute = att1
  *
  * scala> A._att2
  * res1: org.hablapps.updatable.model.Attribute = att2
  * }}}
  *
  * As one can see, after the macro invocation, some new structural members have
  * appeared on val A (''_att1'' & ''_att2''). They are just the attribute
  * reifications which are created dynamically by the macro, and so,
  * ''language.reflectiveCalls'' must be enabled to get things working.
  * 
  * For more information on instantiating this kind of builder, please visit the
  * ''weakBuilder'' (page) documentation section.
  * 
  * @tparam A the abstract entity
  * @see [[org.hablapps.updatable.Builder]]
  * @see [[org.hablapps.updatable.MetaModelAPI]]
  * @see [[org.hablapps.updatable.weakBuilder]]
  */
abstract class WeakBuilder[A: ClassTag: TypeTag] {
  val _class = classTag[A]
  val _ttag = typeTag[A]
 
  /** Collects the attribute reifications for type A. */
  val attributes: List[model.Attribute]

  /** Checks if this type defines the specified attribute */
  def isDefined(attname: String): Boolean = 
    (attributes find { _.name == attname }).isDefined
  
  /** Returns the reification of the attribute serching it by name. */
  def toReification(attname: String): model.Attribute =
    (attributes find { _.name == attname }).get
}

/** Does contain the whole building information of an entity.
  *
  * This builder should be used when dealing with concrete entities, those who
  * can be created and updated. This contains the attribute reifications, just
  * as [[org.hablapps.updatable.Builder]] does, but in addition this offers a
  * range of functions which allow the programmer creating and updating the
  * entity instances.
  *
  * The instances of this class are made by using the macro ''builder[A]''. Next,
  * we will show an example to learn how to create a builder. To find out more
  * details about what the macro is exactly generating, please refer to the
  * ''builder'' macro documentation.
  *
  * {{{
  * scala> import org.hablapps.updatable._
  * import org.hablapps.updatable._
  *
  * scala> import language.reflectiveCalls
  * import language.reflectiveCalls
  *
  * scala> :paste
  * // Entering paste mode (ctrl-D to finish)
  *
  * trait A {
  *   val a1: Int
  * }
  * implicit val A = builder[A]
  *
  * trait B extends A {
  *   val b1: List[String]
  * }
  * implicit val B = builder[B]
  *
  * // Exiting paste mode, now interpreting.
  *
  * defined trait A
  * ...
  * defined trait B
  * ...
  *
  * scala> val a = A.apply(_a1 = 1)
  * a: A = A(a1=1)
  *
  * scala> val b = B.apply(_a1 = a.a1, _b1 = List("x", "y"))
  * b: B = B(a1=1,b1=List(x, y))
  *
  * scala> B.updated(b, B._b1, List("x", "y", "z"))
  * res6: B = B(a1=1,b1=List(x, y, z))
  * }}}
  *
  * @tparam A the abstract entity
  * @see [[org.hablapps.updatable.WeakBuilder WeakBuilder]]
  * @see [[org.hablapps.updatable.MetaModelAPI MetaModelAPI]]
  * @see [[org.hablapps.updatable.builder builder]]
  */
abstract class Builder[A: ClassTag: TypeTag] extends WeakBuilder[A] {

  /** Maps each attribute with its corresponding modifiable. 
    *
    * @see [[org.hablapps.updatable.UnderlyingModifiable]]
    */
  val modifiables: Map[model.Attribute, UnderlyingModifiable]

  def applyNull(): A = ???
  
  /** Returns a default instance.
    *
    * A default instance is the one that sets all it attributes to the default
    * values.
    *
    * @see [[org.hablapps.updatable.Default]]
    */
  def apply(): A = ???

  /** Returns the value set to an attribute from an instance.
    *
    * @param t the entity instance
    * @param a the attribute to get
    */
  private[updatable] def get(t: A, a: RuntimeMetaModel#Attribute): Any

  /** Returns the attribute's extension from an instance.
    *
    * @param t the entity instance
    * @param a the attribute reification
    */
  def extension(t: A, a: model.Attribute): List[Any] = {
    val m = modifiables.get(a)
    if (! m.isDefined)
      List(get(t, a))
    else {
      val mo = m.get
      mo.toList[Any](get(t, a).asInstanceOf[mo.Col[Any]])
    }
  }

  /** Returns an updated entity instance.
    *
    * Creates and returns a new entity instance which copies all the attributes
    * from the original entity but one. The latter is the attribute that appears
    * passed as an input. The value set to that attribute is the incoming ''v''.
    *
    * @param t the original instance
    * @param a the attribute to update
    * @param v the value to set at the previous attribute
    * @return a new instance with the update
    */
  def updated(t: A, a: RuntimeMetaModel#Attribute, v: Any): A

  /** Returns a modified entity instance.
    *
    * Creates and returns a new entity instance which copies all the attributes
    * from the original entity but one. The latter is the attribute that appears
    * passed as an input. The value set to that attribute is the result of
    * modifying it, whether by adding or removing the incoming value, depending
    * on ''mode'' value.
    *
    * This method relies on ''updated''. Before redirecting the invocation it is
    * necessary to retrieve the modifiable associated with the attribute, that
    * empowers the builder to modify it.
    *
    * @param t the original instance
    * @param a the attribute to modify
    * @param v the value to modify
    * @param mode true and false represent positive and negative modification,
    * respectively
    */
  private[updatable] def modify(
      t: A, 
      a: model.Attribute, 
      v: Any, 
      mode: Boolean): A = {
    val m = getModifiable(a)
    updated(t, a, m.modify(get(t, a).asInstanceOf[m.Col[Any]], v, mode))
  }

  /** Returns the modifiable associated with this attribute.
    *
    * @throws NoSuchElementException if no modifiable is found
    */
  private[updatable] def getModifiable(a: model.Attribute): UnderlyingModifiable =
    modifiables.get(a).getOrElse(
      throw new NoSuchElementException(
  s"It seems attribute $a has not got an associated modifiable."))

  /** Returns the modifiable associated with this attribute name. */
  private[updatable] def getModifiable(a: String): UnderlyingModifiable =
    getModifiable(toReification(a))

  /** Returns true if both instances are equals, else false. */
  def _equals(t: A, other: Any): Boolean = other match {
    case T(that) => attributes forall { at => get(t, at) == get(that, at) }
    case _ => false
  }

  /** Returns true if ''t_u'' has the referenced attribute matching ''v''
    * and the rest of them matching ''t'''s attributes.
    *
    * @param t an entity instance
    * @param t_u another entity instance
    * @param a the 'but' attribute
    * @param v the value that previous attribute must match
    */
  private[updatable] def equals_but[C[_], E](
    t: A, 
    t_u: A, 
    a: model.Attribute, 
    v: C[E]): Boolean = attributes.forall { at =>
      if (at == a)
  get(t_u, at) == v
      else
  get(t_u, at) == get(t, at)
    }

  /** Stringifies an entity instance.
   *
   * @param t the entity to stringify
   */
  def show(t: A): String = {
    def show(att: model.Attribute): String = {
      val extension = get(t, att)
      val isEmpty = extension match {
        case n: None.type => true
        case false => true
        case "" => true
        case t: Traversable[_] => t.isEmpty
        case _ => false
      }
      if (! isEmpty)
        att.toString + "=" + (get(t, att) match {
          case Some(s) => s
          case other => other
        })
      else
        ""
    }

    /* TODO: refined types T{ type A = .., type B = .. } should print as 
     * T[A=..,B=..]
     */
    (if (typeOf[A].typeSymbol.name.toString == "<refinement>") 
  typeOf[A].baseClasses(0).typeSignature.typeSymbol.name
      else typeOf[A].typeSymbol.name) + 
    "(" + attributes.map(show(_)).filter(_ != "").mkString(",") + ")"
  }
}
