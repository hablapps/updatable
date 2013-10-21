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

import scala.reflect.macros.Context
import scala.util.matching.Regex

/** Makes textual builder instantiation expressions. */
trait MkBuilder { this: MacroMetaModel =>

  /** The type associated to this builder. */
  val tpe: universe.Type

  /**
   * Returns the attribute reifications.
   *
   * This method iterates by all the attributes existing in `tpe`
   * and generates a new member called `_$attname` for each. If the attribute
   * is declared in the `tpe`, the generated code will instantiate
   * a runtime attribute. Otherwise, it is necessary to search for the builder
   * that is hosting the attribute to point to it.
   */
  private def mkAttReifications = tpe.all map { att =>
    if (att.isDeclared(tpe))
      s"""
      val _${att.name}: model.Attribute = {
        val sym = model.universe.typeOf[${tpe.name}].members.toList.find { s =>
          (s.name.toString == \"${att.name}\") && (s.asTerm.isAccessor)
        }.get
        new model.Attribute(sym) {
          type Owner = ${att.owner.name}
        }
      }
      """
    else if (att.owner.builder(tpe).isDefined)
      s"val _${att.name} = ${att.owner.builder(tpe).get.toString}._${att.name}"
    else {
      c2.warning(
        c2.enclosingPosition,
        s"Can't find a suitable implicit builder, using '${att.owner.name}' instead")
      s"val _${att.name} = ${att.owner.name}._${att.name}"
    }
  } mkString "\n"

  /** Returns an attribute sequence which will be embedded in `attributes`. */
  private def mkAttributes = tpe.all map { att => s"_${att.name}" } mkString ", "

  /**
   * Returns the list of modifiables for all those attributes whose type has a
   * container (simple types cannot be modified).
   */
  private def mkModifiables = {
    for {
      att <- tpe.all;
      if (att.tpe(tpe).isModifiable)
    } yield s"(_${att.name} -> iumodifiable[${clean(att.tpe(tpe).c.get.toString)}])"
  } mkString ", "

  /** Returns the list of parameters with default values. */
  private def mkApplyParams = tpe.all map { att =>
    s"_${att.name}: ${att.tpe(tpe)} = default[${att.tpe(tpe).toString}]"
  } mkString ", "

  /** Returns the list of declarations for the new instance. */
  private def mkApplyVals = tpe.all map { att =>
    s"val ${att.name} = _${att.name}"
  } mkString "\n"

  private def mkEvidenceDefs = tpe.evidences map { ev =>
    val r = ev.returnType
    val sig = s"${r._1}[${r._2}]"
    val lhs = s"implicitly[$sig]"
    s"override def ${ev.name} = $lhs"
  } mkString "\n"

  // private def mkDefaultTpes = tpe.abstractTpesWithDefault map { at =>
  //   s"type ${ at.name } = ${ at.name }$DEFAULT_TYPE_END"
  // } mkString "\n"

  /** Returns the apply method.*/
  private def mkApply =
    s"""
    def apply($mkApplyParams): ${tpe.name} = new ${tpe.name} {
      $mkTypeAliases
      $mkEvidenceDefs
      $mkApplyVals
      override def toString: String = show(this)
      override def equals(other: Any): Boolean = _equals(this, other)
    }
    """

  private def mkApplyNullVals = tpe.all map { att =>
    s"val ${att.name} = null.asInstanceOf[${att.tpe(tpe)}]"
  } mkString "\n"

  private def mkApplyNull =
    s"""
    override def applyNull(): ${tpe.name} = new ${tpe.name} {
      $mkTypeAliases
      $mkEvidenceDefs
      $mkApplyNullVals
      override def toString: String = show(this)
      override def equals(other: Any): Boolean = _equals(this, other)
    }
    """

  /** Returns the cases for the get method. */
  private def mkGetCases = tpe.all map { att =>
    s"case `_${att.name}` => t.${att.name}"
  } mkString "\n"

  /** Returns the arguments that `updated` uses to invoke `apply()`. */
  private def mkUpdApplyArgs = tpe.all map { att =>
    s"""
    _${att.name} = 
      if (a == _${att.name})
        v.asInstanceOf[${att.tpe(tpe).toString}]
      else
        t.${att.name}.asInstanceOf[${att.tpe(tpe).toString}]
    """
  } mkString ", "

  /** Returns the implementation of the method `updated`. */
  private def mkUpdated = s"apply($mkUpdApplyArgs)"

  /** Returns the arguments that default apply uses to invoke `apply`. */
  private def mkDefApplyArgs = tpe.all map { att =>
    s"_${att.name} = default[${att.tpe(tpe).toString}]"
  } mkString ", "

  /** Returns the implementation of the default `apply`. */
  private def mkDefApplyInvocation = s"apply($mkDefApplyArgs)"

  /** Returns an empty instance of `tpe`. */
  private def mkEmptyInstance =
    s"""
    new ${tpe.name} {
      $mkEvidenceDefs
      override def toString: String = show(this)
      override def equals(other: Any): Boolean = _equals(this, other)
    }
    """

  /** Returns an empty string. */
  private def mkNothing = ""

  /**
   * Returns a `WeakBuilder` empowered to build `tpe`s.
   *
   * Supposing we have the next entity hierarchy:
   *
   * {{{
   * trait A {
   *   val a1: Int
   *   val a2: String
   * }
   *
   * trait B extends A {
   *   type B1
   *   val b1: B1
   * }
   * }}}
   *
   * The code that would be generated after invoking `weakBuilder[B]` is the
   * following:
   *
   * {{{
   * new WeakBuilder[B] {
   *   val _a1 = A._a1
   *   val _a2 = A._a2
   *   val _b1: model.Attribute = {
   *     val sym = model.universe.typeOf[B].members.toList.find { s =>
   *        (s.name.toString == "b1") && (s.asTerm.isAccessor)
   *      }.get
   *      new model.Attribute(sym) {
   *        type Owner = B
   *      }
   *   }
   *   val attributes: List[model.Attribute] = List(_a1, _a2, _b1)
   * }
   * }}}
   */
  def mkWeakBuilder =
    s"""
    new WeakBuilder[${tpe.name}] {
      $mkAttReifications
      val attributes: List[model.Attribute] = List($mkAttributes)
    }
    """

  def mkDummyBuilder =
    s"""
    new Builder[${tpe.name}] {
      $mkAttReifications
      val attributes: List[model.Attribute] = List($mkAttributes)
      val modifiables: Map[model.Attribute, UnderlyingModifiable] = Map()

      def get(t: ${tpe.name}, a: RuntimeMetaModel#Attribute): Any = ???

      def updated(
        t: ${tpe.name}, 
        a: RuntimeMetaModel#Attribute, 
        v: Any): ${tpe.name} = ???
    }
    """

  def mkTypeAliases = tpe.defaultTpeValMap map { kv =>
    s"type ${ kv._1 } = ${ kv._2 }"
  } mkString "\n"

  /**
   * Returns a `Builder` empowered to build `tpe`s.
   *
   * Supposing we have the next entity hierarchy:
   *
   * {{{
   * trait A {
   *   val a1: Int
   *   val a2: String
   * }
   *
   * trait B extends A {
   *   val b1: Option[Int]
   * }
   * }}}
   *
   * The code that would be generated after invoking `builder[B]` is the
   * following:
   *
   * {{{
   * new Builder[B] {
   *   val _a1 = A._a1
   *   val _a2 = A._a2
   *   val _b1: model.Attribute = {
   *     val sym = model.universe.typeOf[B].members.toList.find { s =>
   *       (s.name.toString == "b1") && (s.asTerm.isAccessor)
   *     }.get
   *     new model.Attribute(sym) { type Owner = B }
   *   }
   *   val attributes: List[model.Attribute] = List(_a1, _a2, _b1)
   *   val modifiables: Map[model.Attribute, UnderlyingModifiable] =
   *     Map((_b1 -> iumodifiable[Option]))
   *
   *   def apply(
   *       _a1: Id[Int] = default[Id[Int]],
   *       _a2: Id[String] = default[Id[String]],
   *       _b1: Option[Int] = default[Option[Int]]): B = new B {
   *     val a1 = _a1
   *     val a2 = _a2
   *     val b1 = _b1
   *     override def toString: String = show(this)
   *     override def equals(other: Any): Boolean = _equals(this, other)
   *   }
   *
   *   override def apply(): B =
   *     apply(
   *       _a1 = default[Id[Int]],
   *       _a2 = default[Id[String]],
   *       _b1 = default[Option[Int]])
   *
   *   def get(t: B, a: model.Attribute): Any = a match {
   *     case `_a1` => t.a1
   *     case `_a2` => t.a2
   *     case `_b1` => t.b1
   *     case _ => {
   *       throw new MatchError("Attribute " + a + " does not exist")
   *     }
   *   }
   *
   *   def updated(t: B, a: model.Attribute, v: Any): B =
   *      apply(
   *       _a1 = if (a == _a1) v.asInstanceOf[Id[Int]] else t.a1,
   *       _a2 = if (a == _a2) v.asInstanceOf[Id[String]] else t.a2,
   *       _b1 = if (a == _b1) v.asInstanceOf[Option[Int]]else t.b1)
   * }
   * }}}
   */
  def mkBuilder = {
    if (tpe.isAbstract) {
      val absTpes =
        (tpe.abstractTpesWithoutDefault map { _.name.decoded }) mkString ", "
      val defTpes =
        (tpe.abstractTpesWithDefault map { _.name.decoded }) mkString ", "
      c2.warning(
        c2.enclosingPosition,
        s"""
        | ${tpe.name} is an abstract type. Are you sure you are not willing to use a 'weakBuilder[${tpe.name}]' instead?.
        | - Abstract types: $absTpes
        | - Abstract types (with default): $defTpes
        """.stripMargin)
    }
    s"""
    new Builder[${tpe.name}] {
      $mkAttReifications
      val attributes: List[model.Attribute] = List($mkAttributes)
      val modifiables: Map[model.Attribute, UnderlyingModifiable] = 
        Map($mkModifiables)
  
      $mkApplyNull

      ${if (!tpe.all.isEmpty) mkApply else mkNothing}

      override def apply(): ${tpe.name} = ${
        if (tpe.all.isEmpty) mkEmptyInstance else mkDefApplyInvocation
      }
      def get(t: ${tpe.name}, a: RuntimeMetaModel#Attribute): Any = a match {
        $mkGetCases
        case _ => {
          throw new MatchError(\"Attribute \" + a + \" does not exist\")
        }
      }
      def updated(
        t: ${tpe.name}, 
        a: RuntimeMetaModel#Attribute, 
        v: Any): ${tpe.name} = $mkUpdated
    }
    """
  }
}

trait MkAtBuilder { this: MacroMetaModel =>
  import universe._
  import Flag._

  val entity: universe.Type

  def mkObjectConstructor =
    q"def ${nme.CONSTRUCTOR}() = { super.${nme.CONSTRUCTOR}(); () }"

  def mkModifiablesVal =
    q"lazy val modifiables: Map[org.hablapps.updatable.model.Attribute, org.hablapps.updatable.UnderlyingModifiable] = ???"

  private def mkLocalAttReifs: List[ValDef] = entity.declared map { att =>
    q"""
    val ${newTermName("_" + att.name)}: model.Attribute = {
      val sym = model.universe.weakTypeOf[${entity.name}].members.toList.find { s =>
        (s.name.toString == ${att.name.decoded}) && (s.asTerm.isAccessor)
      }.get
      new model.Attribute(sym) {
        type Owner = ${entity.name}
      }
    }
    """
  }

  private def mkInheritedAttReifs: List[ValDef] = entity.inherited map { att =>
    q"""val ${newTermName("_" + att.name)} = 
      ${newTermName(att.owner.name.toString)}.builder.${newTermName("_" + att.name)}"""
  }

  def mkAttributeReifications: List[ValDef] = mkLocalAttReifs ::: mkInheritedAttReifs

  def mkAttributes: List[Tree] = entity.all map (att => q"""${newTermName("_" + att.toString)}""")

  def mkAttributesVal =
    q"lazy val attributes: List[org.hablapps.updatable.model.Attribute] = List(..$mkAttributes)"

  lazy val applyStuff: (List[ValDef], List[ValDef]) = 
    (entity.all map { att =>
      val atpe = att.tpe(entity).tpe
      (q"""val ${newTermName("_" + att.toString)}: $atpe""",
        q"""val ${att.name} = ${newTermName("_" + att.toString)}""")
    }).unzip

  def mkApplyMethod =
    q"""def apply(..${applyStuff._1}): ${entity.name} = new ${entity.name} { 
      ..${applyStuff._2}
      override def toString: String = show(this)
      override def equals(other: Any): Boolean = _equals(this, other)
    }"""

  def mkDefApplyMethod =
    if (entity.all.isEmpty)
      q"override def apply(): ${entity.name} = new ${entity.name} {}"
    else {
      val args = entity.all map (att => q"default[${att.tpe(entity).tpe}]")
      q"override def apply(): ${entity.name} = apply(..$args)"
    }

  def mkGetMethod =
    q"def get(t: ${entity.name}, a: org.hablapps.updatable.RuntimeMetaModel#Attribute): Any = ???"

  def mkUpdatedMethod =
    q"def updated(t: ${entity.name}, a: org.hablapps.updatable.RuntimeMetaModel#Attribute, v: Any): ${entity.name} = ???"

  lazy val newObjectBody: List[Tree] = List(
    mkObjectConstructor,
    mkModifiablesVal,
    mkAttributesVal,
    if (entity.all.isEmpty) EmptyTree else mkApplyMethod,
    mkDefApplyMethod,
    mkGetMethod, 
    mkUpdatedMethod) ::: mkAttributeReifications

  lazy val newObjectTemplate = Template(
    List(tq"org.hablapps.updatable.Builder[${entity.name}]"),
    emptyValDef,
    newObjectBody)

  lazy val newObjectDef = ModuleDef(
    Modifiers(IMPLICIT), 
    newTermName("builder"),
    newObjectTemplate)

  def mkAttributeReificationAccessors: List[ValDef] = entity.all map { att =>
    val name = newTermName("_" + att.name.toString)
    q"""val $name: model.Attribute = builder.$name"""
  }

  def mkDefApplyAccessor: DefDef =
    q"def apply() = builder.apply()"

  def mkAttributesAccessor: ValDef = 
    q"val attributes = builder.attributes"

  def mkApplyAccessor: DefDef = {
    lazy val args = entity.all map { att => 
      q"""${newTermName("_" + att.toString)}"""
    }
    q"def apply(..${applyStuff._1}) = builder.apply(..$args)"
  }

  def mkAccessors: List[Tree] = mkAttributesAccessor :: 
    (if (entity.all.isEmpty) EmptyTree else mkApplyAccessor) :: 
    mkDefApplyAccessor ::
    mkAttributeReificationAccessors

  def apply = {
    println("#####> " + Block(newObjectDef :: mkAccessors, Literal(Constant(()))))
    c2.Expr[Any](Block(newObjectDef :: mkAccessors, Literal(Constant(()))))
  }

  /* Weak Builder */

  def mkWeakModifiablesVal =
    q"lazy val modifiables: Map[org.hablapps.updatable.model.Attribute, org.hablapps.updatable.UnderlyingModifiable] = ???"

  def mkWeakGetMethod =
    q"def get(t: ${entity.name}, a: org.hablapps.updatable.RuntimeMetaModel#Attribute): Any = ???"

  def mkWeakUpdatedMethod =
    q"def updated(t: ${entity.name}, a: org.hablapps.updatable.RuntimeMetaModel#Attribute, v: Any): ${entity.name} = ???"

  lazy val weakObjectBody: List[Tree] = List(
    mkObjectConstructor,
    mkWeakModifiablesVal,
    mkAttributesVal,
    mkWeakGetMethod, 
    mkWeakUpdatedMethod) ::: mkAttributeReifications

  lazy val weakObjectTemplate = Template(
    List(tq"org.hablapps.updatable.Builder[${entity.name}]"), 
    emptyValDef, 
    weakObjectBody)

  lazy val weakObjectDef = ModuleDef(
    Modifiers(IMPLICIT), 
    newTermName("builder"), 
    weakObjectTemplate)

  def mkWeakAccessors: List[Tree] =
    mkAttributesAccessor :: mkAttributeReificationAccessors

  def weak = {
    println("##w##> " + weakObjectDef)
    c2.Expr[Any](Block(weakObjectDef :: mkWeakAccessors, Literal(Constant(()))))
  }
}
