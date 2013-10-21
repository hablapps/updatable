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

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.{ classTag, ClassTag }
import scala.reflect.runtime.universe._

object `package` {

  class alias(
      val attribute: String, 
      val filter: String, 
      val asCollection: String)
    extends StaticAnnotation

  class Value extends StaticAnnotation

  /* Given a trait A annotated with @builder:
   * {{{
   * @builder trait A
   * }}}
   *
   * this macro generates:
   * {{{
   * trait A
   * 
   * object A {
   *   @innerBuilder type A1 = A
   * }
   * }}}
   */
  class builder extends StaticAnnotation { 
    def macroTransform(annottees: Any*) = macro Macros.macroAtBuilderImpl
  }

  class weakBuilder extends StaticAnnotation { 
    def macroTransform(annottees: Any*) = macro Macros.macroAtWeakBuilderImpl
  }

  class innerBuilder extends StaticAnnotation { 
    def macroTransform(annottees: Any*) = 
      macro Macros.macroAtInnerBuilderImpl
  }

  class weakInnerBuilder extends StaticAnnotation { 
    def macroTransform(annottees: Any*) = 
      macro Macros.macroAtWeakInnerBuilderImpl
  }

  object JSAnnots {
    type value = org.hablapps.updatable.Value @scala.annotation.meta.getter
  }

  class default(value: String) extends StaticAnnotation

  def isEquals[A, B](implicit e: A =:= B = null) = e ne null

  implicit def toO[V, W](implicit e: V <:< W = null) = Option(e)

  case class type_==[B]() {
    def is[A](t: => A)(implicit e: A =:= B = null) = e ne null
  }

  case class type_<=[B]() {
    def is[A](t: => A)(implicit e: A <:< B = null) = e ne null
  }

  object value {
    def apply[A] = null.asInstanceOf[A]
    def apply[A](t: Any) = t.asInstanceOf[A]
  }

  object T {
    def unapply[T: ClassTag](that: Any): Option[T] = {
      val thisInterface = classTag[T].runtimeClass
      val thatInterfaces = that.getClass.getInterfaces
      if (thisInterface.isInterface &&
        thatInterfaces.length == 1 &&
        (thisInterface equals thatInterfaces(0)))
        Some(that.asInstanceOf[T])
      else
        None
    }
  }

  type Id[A] = A

  class Union[X, Y] extends annotation.Annotation

  class Union3[X, Y, Z] extends annotation.Annotation

  class Union4[X, Y, Z, V] extends annotation.Annotation

  case class PosInfo(file: String, line: Int, lineContent: String, show: String) {
    override def toString = s"$file:$line"
  }

  object PosInfo {
    implicit def getPos: PosInfo = macro Macros.getPosImpl
  }

  /**
   * Returns a suitable updatable for the entity value `t`.
   *
   * @tparam A  the entity type
   * @param t the entity value to wrap
   * @param e the builder to wrap
   */
  implicit def toUpdatable[A](t: A)(implicit e: Builder[A]): Updatable[A] =
    new {
      val builder = implicitly[Builder[A]]
    } with Updatable[A] {
      type Tpe = A
      val value = t
    }

  /**
   * Returns the value wrapped in the updatable `u`.
   *
   * @tparam S the entity type
   * @param u the updatable instance
   */
  implicit def fromUpdatable[S](u: Updatable[S]): S = u.value

  /**
   * Returns the default value associated to type `A`.
   *
   * @tparam A the type for which we are searching a default
   * @param e an implicit option value containing the default
   */
  def default[A](implicit e: Option[Default[A]] = None): A =
    try {
      e.get.value
    } catch {
      case _: NoSuchElementException => {
        throw new NoSuchElementException(
          "Cannot find a default value for type A")
      }
    }

  def higherDefault[C[_], T](implicit e: HigherDefault[C] = null): C[T] =
    try { e.value[T] } catch {
      case _: NullPointerException =>
        throw new NoSuchElementException("Cannot find a default value for type constructor C")
    }

  /**
   * Returns new default values.
   *
   * @tparam A the type of the default
   * @param v the default value to set
   */
  def toDefault[A](v: A): Default[A] = new Default[A] {
    def value = v
  }

  /**
   * Lifts an implicit `Default[T]` to an `Option[Default[T]]`. This avoids
   * dealing with possible null pointers.
   */
  implicit def toDefaultOption[T](implicit ev: Default[T]): Option[Default[T]] =
    Option(implicitly[Default[T]])

  trait EntityTag

  trait EvidenceTag

  /**
   * Returns a mapping of attribute reifications as keys and evidences
   * `E[attTpe]` as values.
   *
   * @tparam E the evidence the invoker is interested in
   * @tparam T the type of the entity
   * @param b the required builder to extract the attribute reifications
   */
  def attributeEvidences[E, T,V]: Map[String, EvidenceTag] = macro Macros.attributeEvidencesImpl[E, T, V]

  def fAttributeEvidences[E, T,V]: Map[String, EvidenceTag] = macro Macros.fAttributeEvidencesImpl[E, T, V]

  /**
   * Does contain common information about attribute helpers, such as
   * the `attribute` and the `updatable`.
   */
  trait CommonHelper {
    type T

    val attribute: String
    val updatable: Updatable[T]
  }

  /** Does allow the `entity.attribute := value` sintax. */
  trait UpdatedHelper extends CommonHelper {
    type CE

    def :=(value: CE): Updatable[T] = updatable.updated(attribute, value)
  }

  /**
   * Factory for [[org.hablapps.updatable.UpdatedHelper]].
   *
   * Not a big deal, but simplifies notably the work inside `toUpdatedHelper`.
   */
  object UpdatedHelper {
    def apply[A, B](_attribute: String, _updatable: Updatable[A]) =
      new UpdatedHelper {
        type T = A
        type CE = B

        val attribute = _attribute
        val updatable = _updatable
      }
  }

  /** Does allow the `entity.attribute [+/-]= element` sintax. */
  trait ModifyHelper extends CommonHelper {
    type E

    def +=(value: E): Updatable[T] = updatable.modify(attribute, value, true)

    def -=(value: E): Updatable[T] = updatable.modify(attribute, value, false)
  }

  /**
   * Factory for [[org.hablapps.updatable.ModifyHelper]].
   *
   * Not a big deal, but simplifies notably the work inside `toModifyHelper`.
   */
  object ModifyHelper {
    def apply[A, B](_attribute: String, _updatable: Updatable[A]) =
      new ModifyHelper {
        type T = A
        type E = B

        val attribute = _attribute
        val updatable = _updatable
      }
  }

  /**
   * Returns an `UpdatedHelper` from the expression `entity.attribute`.
   *
   * This implicit declaration empowers the user to turn an `entity.attribute`
   * expression into an `UpdatedHelper`. This is not an easy translation at
   * all. Notice that it is a need to extract the attribute reification, the
   * updatable and the involved types, to make the operation type safe. For
   * this reason, a macro is required.
   *
   * Firstly the macro parses the input. It must take into account two
   * different situations:
   *
   * 1. The entity accesses directly to its attribute:
   * {{{
   * scala> import org.hablapps.updatable._
   * import org.hablapps.updatable._
   *
   * scala> trait A { val a1: Int; val a2: List[String] }; implicit val A = builder[A]
   * defined trait A
   * ...
   *
   * scala> A().a1 := 5
   * res0: org.hablapps.updatable.Updatable[A] = A(a1 = 5)
   *
   * scala> A().a2 := List("x", "y", "z")
   * res1: org.hablapps.updatable.Updatable[A] = A(a1 = 0, a2 = List(x, y, z))
   * }}}
   *
   * In this case, the macro does not have the `updatable` available, so it
   * searchs for a `Builder[A]` in scope to extract the attribute reification.
   * Besides, to generate the `updatable`, it invokes the `toUpdatable` on the
   * entity manually.
   *
   * 2. The updatable accesses the attribute (by applying the view
   * `fromUpdatable`):
   * {{{
   * scala> val updatable = Updatable.apply(A())
   * updatable: org.hablapps.updatable.Updatable[A] = A(a1 = 0)
   *
   * scala> updatable.a1 := 4
   * res2: org.hablapps.updatable.Updatable[A] = A(a1 = 4)
   *
   * scala> updatable.a2 := List("a", "b", "c")
   * res3: org.hablapps.updatable.Updatable[A] = A(a1 = 0, a2 = List(a, b, c))
   * }}}
   *
   * This case requires a harder parsing, because though it cannot be
   * seen, the view `fromUpdatable` is being applicated. However, it is not
   * needed extra work to get the updatable, as was needed in the former
   * situation.
   *
   * Finally, it worth mentioning that the attribute reification is located
   * given that its name appears in the input expression. For instance, if
   * the input is `example.attname`, this method assumes that the `builder`
   * contains an attribute reification `builder._attname`.
   *
   * @tparam V the type of the attribute type
   * @param v the current value for `entity.attribute`
   */
  implicit def toUpdatedHelper[V](v: V): UpdatedHelper = macro Macros.toAttributeHelperImpl[V, UpdatedHelper]

  /**
   * Returns a `ModifyHelper` from the expression `entity.attribute`.
   *
   * This implicit declaration empowers the user to turn an `entity.attribute`
   * expression into a `ModifyHelper`. Some basic usage examples are shown
   * below:
   *
   * {{{
   * scala> import org.hablapps.updatable._
   * import org.hablapps.updatable._
   *
   * scala> trait A { val a1: Int; val a2: List[String] }; implicit val A = builder[A]
   * defined trait A
   * ...
   *
   * scala> A().a2 += "x"
   * res0: org.hablapps.updatable.Updatable[A] = A(a1=0,a2=List(x))
   *
   * scala> (((res0.a2 += "y").a2 += "y").a2 += "z").a2 += "x"
   * res1: org.hablapps.updatable.Updatable[A] = A(a1=0,a2=List(x, y, y, z, x))
   *
   * scala> (res1.a2 -= "x").a2 -= "y"
   * res2: org.hablapps.updatable.Updatable[A] = A(a1=0,a2=List(z))
   * }}}
   *
   * Notice that a simple attribute, such as `a1` cannot be modified. To
   * replace the value of a simple attribute, you must use `updated`
   * instead.
   *
   * Please, refer to [[org.hablapps.updatable.toUpdatedHelper]] if you are
   * interested in how the input expression is parsed, because it is exactly
   * the same that this uses.
   */
  implicit def toModifyHelper[V](v: V): ModifyHelper = macro Macros.toAttributeHelperImpl[V, ModifyHelper]

  /** Runtime metamodel. */
  val model = RuntimeMetaModel.apply

  private def genFinder(tpeA: model.universe.Type): model.Model = {
    import model._

    def getModelType(tr: model.universe.Type) = tr match {
        case TypeRef(_, _, List(tpe @ TypeRef(pre, _, _))) =>
          tpe.asSeenFrom(tpeA, pre.typeSymbol.asClass)
        case RefinedType(List(
          TypeRef(_, _, List(tpe @ TypeRef(pre, _, _)))), _) => {
          tpe.asSeenFrom(tpeA, pre.typeSymbol.asClass)
        }
        case _ => throw new Error(s"Can't process type ${ showRaw(tr) }")
      }

    def whole: List[model.universe.Type] = (for {
      m <- tpeA.members;
      if m.isMethod && m.asMethod.isGetter;
      if m.asMethod.returnType <:< typeOf[Builder[_]];
      if !(m.asMethod.returnType =:= typeOf[Null])
    } yield getModelType(m.asMethod.returnType)).toList.reverse

    def altWhole: List[model.universe.Type] = (for {
      m <- tpeA.members;
      if m.isTerm && m.asTerm.isVal;
      if m.asTerm.typeSignature <:< typeOf[Builder[_]];
      if !(m.asTerm.typeSignature =:= typeOf[Null])
    } yield getModelType(m.asTerm.typeSignature)).toList.reverse

    val w = whole
    new model.Model(tpeA.toString, if (w.isEmpty) altWhole else w)
  }

  /**
   * Finds all the members (as types) of a model from a System.
   *
   * Returns the model deployed at the system. Firstly, this searches all
   * the builders in the system. Then, it extracts the type of the builder
   * '''as seen from''' the system. Finally, the model is generated and
   * returned.
   *
   * @tparam the system type
   * @return the model deployed at the system
   */
  def finder[A: TypeTag]: model.Model = genFinder(typeOf[A])

  def finder(sys: Any): model.Model = {
    import model._

    val m = universe.runtimeMirror(sys.getClass.getClassLoader)
    val im = m.reflect(sys)
    genFinder(im.symbol.toType)
  }

  def newAttribute[A: TypeTag](attname: String) = {
    val sym = model.universe.typeOf[A].members.toList.find { s =>
      (s.name.toString == attname) && (s.asTerm.isAccessor)
    }.get
    new model.Attribute(sym) {
      type Owner = A
    }
  }

  /**
   * Returns a `WeakBuilder` for `A`.
   *
   * Briefly, a `WeakBuilder` is a reduced version of `Builder`, whose main
   * mission is to keep the attribute reifications within.
   *
   * Usage example:
   *
   * {{{
   * scala> import org.hablapps.updatable._
   * import org.hablapps.updatable._
   *
   * scala> trait A { type A1; val a1: A1; val a2: Int }; implicit val A = weakBuilder[A]
   * defined trait A
   * ...
   *
   * scala> import language.reflectiveCalls
   * import language.reflectiveCalls
   *
   * scala> A._a1
   * res0: org.hablapps.updatable.model.Attribute = a1
   *
   * scala> A._a2
   * res1: org.hablapps.updatable.model.Attribute = a2
   * }}}
   *
   * The last two evaluations show that the attribute reifications are
   * generated as expected.
   *
   * @tparam A the input type to generate the builder
   * @see [[org.hablapps.updatable.WeakBuilder WeakBuilder]]
   */
  //def weakBuilder[A] = macro Macros.weakBuilderImpl[A]
  def weakBuilder[A] = macro Macros.dummyBuilderImpl[A]

  /**
   * Returns a `Builder` for `A`.
   *
   * Briefly, a `Builder` is the mechanism that allow instantiating new
   * entities, updating or modifying them, and besides, it keeps
   * metainformation such as the attribute reifications and the modifiables
   * for each attribute.
   *
   * Usage example:
   *
   * {{{
   * scala> trait A { type A1Col[_]; type A1; val a1: A1Col[A1]; val a2: Int };
   *        implicit val A = weakBuilder[A] // A is abstract
   * defined trait A
   * ...
   *
   * scala> trait B extends A { type A1Col[x] = Option[x]; type A1 = Int; val b1: Double };
   *        implicit val B = builder[B] // B concretes A's abstract members
   * defined trait B
   * ...
   *
   * scala> B(_a1 = Option(5), _b1 = 2.0)
   * res0: B = B(a1 = 5, a2 = 0, b1 = 2.0)
   *
   * scala> res0.a1 -= 5
   * res1: org.hablapps.updatable.Updatable[B] = B(a2 = 0,b1 = 2.0)
   * }}}
   *
   * @tparam A the input type to generate the builder
   * @see [[org.hablapps.updatable.Builder Builder]]
   * @see [[org.hablapps.updatable.MkBuilder MkBuilder]]
   */
  def builder[A] = macro Macros.builderImpl[A]

  /**
   * Returns an implicit `Builder` in scope for type `A`.
   *
   * @tparam A the builder associated type
   * @param e the implicit evidence of the builder
   */
  def ibuilder[A](implicit e: Builder[A]) = e

  def iweakBuilder[A](implicit e: WeakBuilder[A]) = e

  type Modifiable[A[_]] = UnderlyingModifiable { type Col[x] = A[x] }

  def imodifiable[C[_]: Modifiable]: Modifiable[C] = implicitly[Modifiable[C]]

  def iumodifiable[C[_]: Modifiable]: UnderlyingModifiable =
    implicitly[Modifiable[C]]

  // FILTER * => * WITH CLASS

  def withElemClass[C[_]: WithElemClass] = implicitly[WithElemClass[C]]

  implicit def classOfElements[C[_]: WithElemClass](i: C[_]) =
    withElemClass[C].classOfElements(i)

  implicit def filterConsWithClass[C1[_]: Modifiable, C2[_]: WithElemClass, V](
    c: C1[C2[V]]) = new FilterConsWithClass(c)

  //Enumerated trait for case objects.
  trait Enumerated

  def getBuilderNameByInstance(a: Any): String = try {
    a.getClass.getInterfaces.head.getName.split("\\$").reverse.head
  } catch {
    case _: Throwable => ""
  }

  //  get singleton object

  def getObjectAs[T](s: String)(component: Object): T = {
    val (singleton, obj, inst) = (
      component.getClass.getName.split(""" """).last,
      s.replace(""""""", ""),
      component)
    getObject(singleton, obj, inst).asInstanceOf[T]
  }

  def getObject(_singleton: String, _object: String, instance: Any): Object = {
    val methods = Class.forName(_singleton).getDeclaredMethods.map(_.getName)
    val eqMethods = methods.filter(m => m == _object).head
    Class.forName(_singleton).getDeclaredMethods
      .filter(m => m.getName == _object).head
      .invoke(instance)
  }

  implicit def optBuilder[T: Builder]: Option[Builder[T]] = Some(implicitly[Builder[T]])

}

//  Exception

class UpdatableException(
  val msg: String,
  val code: String = "UPDATABLE - Undefined",
  val causedBy: Option[Throwable] = None) extends Exception(msg) {

  def getTrace: List[Throwable] = causedBy match {
    case Some(cause) => cause match {
      case c: UpdatableException => List(this) ::: c.getTrace
      case e => List(this, e)
    }
    case None => List(this)
  }

  def printTrace {
    println(this + "\n...caused by:")
    for (c <- getTrace) println(c)
  }

  def canEqual(other: Any) = {
    other.isInstanceOf[UpdatableException]
  }

  override def equals(other: Any) = {
    other match {
      case that: UpdatableException => that.canEqual(this) && msg == that.msg && code == that.code && causedBy == that.causedBy
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime + msg.hashCode + code.hashCode + causedBy.hashCode
  }

  override def toString = "[" + code + "] : " + msg
}
