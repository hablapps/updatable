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

trait MkAtBuilder { this: MacroMetaModel =>
  import universe._
  import Flag._

  val entity: universe.Type

  def mkObjectConstructor =
    q"def ${nme.CONSTRUCTOR}() = { super.${nme.CONSTRUCTOR}(); () }"

  def mkModifiablesVal = {

    def mkTuples = for {
      att <- entity.all;
      if att.tpe(entity).isModifiable
    } yield q"""${newTermName("_" + att.toString)} -> iumodifiable[${att.tpe(entity).tpe.typeConstructor}]"""

    q"lazy val modifiables: Map[org.hablapps.updatable.model.Attribute, org.hablapps.updatable.UnderlyingModifiable] = Map(..$mkTuples)"
  }

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

  def mkTypeAliases: List[TypeDef] = entity.defaultNamesMap.toList map { kv =>
    if (kv._2.takesTypeArgs)
      q"type ${kv._1}[x] = ${kv._2.typeSymbol.name.toTypeName}[x]"
    else
      q"type ${kv._1} = ${kv._2}"
  }

  def mkEvidenceDefs: List[DefDef] = entity.evidences map { ev =>
    q"override def ${ev.name}: ${ev.returnType} = implicitly[${ev.returnType}]"
  }

  lazy val applyStuff: (List[ValDef], List[ValDef]) = 
    (entity.all map { att =>
      val atpe = att.tpe(entity).tpe
      (q"""val ${newTermName("_" + att.toString)}: $atpe""",
        q"""val ${att.name} = ${newTermName("_" + att.toString)}""")
    }).unzip

  def mkApplyMethod =
    q"""def apply(..${applyStuff._1}): ${entity.name} = new ${entity.name} {
      ..$mkTypeAliases
      ..$mkEvidenceDefs
      ..${applyStuff._2}
      override def toString: String = show(this)
      override def equals(other: Any): Boolean = _equals(this, other)
    }"""

  def mkDefApplyMethod =
    if (entity.all.isEmpty)
      q"""override def apply(): ${entity.name} = new ${entity.name} {
        ..$mkTypeAliases
        ..$mkEvidenceDefs
        override def toString: String = show(this)
        override def equals(other: Any): Boolean = _equals(this, other)
      }"""
    else {
      val args = entity.all map (att => q"default[${att.tpe(entity).tpe}]")
      q"override def apply(): ${entity.name} = apply(..$args)"
    }

  def mkApplyNullMethod: DefDef = {

    def mkApplyNullVals = entity.all map { att =>
      q"val ${att.name} = null.asInstanceOf[${att.tpe(entity).tpe}]"
    }

    q"""override def applyNull(): $entity = new $entity {
      ..$mkTypeAliases
      ..$mkEvidenceDefs
      ..$mkApplyNullVals
      override def toString: String = show(this)
      override def equals(other: Any): Boolean = _equals(this, other)
    }"""
  }

  def mkGetMethod = {

    def mkCases: List[CaseDef] = entity.all map { att =>
      cq"`${newTermName("_" + att.toString)}` => t.${att.name}"
    }

    def mkMatch = Match(Ident(newTermName("a")), mkCases)

    q"""def get(
      t: ${entity.name}, 
      a: org.hablapps.updatable.RuntimeMetaModel#Attribute): Any = $mkMatch"""
  }

  def mkUpdatedMethod = {

    def mkUpdatedApplyArgs: List[Tree] = entity.all map { att =>
      q"""if (a == this.${newTermName("_" + att.toString)})
        v.asInstanceOf[${att.tpe(entity).tpe}]
      else
        t.${att.name}.asInstanceOf[${att.tpe(entity).tpe}]"""
    }

    q"""def updated(
      t: ${entity.name}, 
      a: org.hablapps.updatable.RuntimeMetaModel#Attribute, 
      v: Any): ${entity.name} = apply(..$mkUpdatedApplyArgs)"""
  }

  lazy val newObjectBody: List[Tree] = List(
    mkObjectConstructor,
    mkModifiablesVal,
    mkAttributesVal,
    if (entity.all.isEmpty) EmptyTree else mkApplyMethod,
    mkDefApplyMethod,
    mkApplyNullMethod,
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

  def mkApplyNullAccessor: DefDef =
    q"def applyNull(): $entity = builder.applyNull()"

  def mkGetAccessor: DefDef =
    q"""def get(
        t: ${entity.name}, 
        a: org.hablapps.updatable.RuntimeMetaModel#Attribute): Any =
      builder.get(t, a)"""

  def mkUpdatedAccessor: DefDef =
    q"""def updated(
        t: ${entity.name}, 
        a: org.hablapps.updatable.RuntimeMetaModel#Attribute, 
        v: Any): ${entity.name} = builder.updated(t, a, v)"""

  def mkAccessors: List[Tree] = mkAttributesAccessor :: 
    (if (entity.all.isEmpty) EmptyTree else mkApplyAccessor) ::
    mkApplyNullAccessor ::
    mkDefApplyAccessor ::
    mkGetAccessor ::
    mkUpdatedAccessor ::
    mkAttributeReificationAccessors

  def apply = {
    //println("#####> " + Block(newObjectDef :: mkAccessors, Literal(Constant(()))))
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
    //println("##w##> " + weakObjectDef)
    c2.Expr[Any](Block(weakObjectDef :: mkWeakAccessors, Literal(Constant(()))))
  }
}
