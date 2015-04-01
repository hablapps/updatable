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
import language.experimental.macros

/** Contains some macro implementations. */
object Macros {

  /**
   * Returns the expression that creates the helper.
   *
   * Parses the input to extract the involved entity, updatable and attribute.
   * Then, the instruction that instantiates the helper is created, taking
   * care of the type that must be returned, either UpdatedHelper or
   * ModifyHelper. Finally, the instruction is hand crafted and returned to
   * the user.
   *
   * For invocation and usage examples, please refer to the declaration
   * documentation at [[org.hablapps.updatable.toUpdatedHelper]] and
   * [[org.hablapps.updatable.toModifyHelper]] instead.
   *
   * @tparam V the type of the value
   * @tparam H the expected kind of helper that this should return
   * @param c the macro context
   * @param v the value expression
   * @return the expression that creates the helper instance
   */
  def toAttributeHelperImpl[V: c.WeakTypeTag, H: c.WeakTypeTag](
    c: Context)(v: c.Expr[V]): c.Expr[H] = {
    import c.mirror._
    import c.universe._

    val hTpe = weakTypeTag[H].tpe
    val vTpe = weakTypeTag[V].tpe

    def parseInput = v.tree match {
      case Select(qua @ Apply(
        TypeApply(Select(_, view), _),
        List(updatable)), sel) if view.toString == "fromUpdatable" => {
        (qua, updatable, Literal(Constant(sel.toString)))
      }
      case Select(qua, sel) => {
        val builder = c.inferImplicitValue(
          appliedType(c.universe.typeOf[Builder[_]], List(qua.tpe.widen)), false)
        val updatable =
          Apply(Apply(Ident(newTermName("toUpdatable")), List(qua)), List(builder))

        (qua, updatable, Literal(Constant(sel.decoded)))
      }
      case _ => {
        c.abort(
          c.enclosingPosition,
          s"Expression must match 'Select(ent, att)', found: ${showRaw(v.tree)}")
      }
    }

    val (entity, updatable, attribute) = parseInput
    val _tpe = hTpe match {
      case t if (t =:= c.universe.typeOf[ModifyHelper]) => vTpe match {
        case TypeRef(_, _, args) if (args.length > 0) => args(0).widen
        case _ => {
          c.abort(
            c.enclosingPosition,
            s"Simple attributes, like '$attribute', cannot be modified")
        }
      }
      case t if (t =:= c.universe.typeOf[UpdatedHelper]) => vTpe.widen
    }

    def isFinalType(t: universe.Type): Boolean = {

      def cond = entity.tpe.members.toList.contains(t.typeSymbol) ||
        t.widen.typeSymbol.isFinal

      if (t.typeConstructor.takesTypeArgs)
        cond && isFinalList(t.asInstanceOf[universe.TypeRef].args)
      else
        cond
    }

    def isFinalList(args: List[universe.Type]): Boolean =
      (args find { !isFinalType(_) }).isEmpty

    c.Expr[H](
      Apply(
        TypeApply(
          Select(
            (if (hTpe =:= c.universe.typeOf[UpdatedHelper])
              Ident(newTermName("UpdatedHelper"))
            else if (hTpe =:= c.universe.typeOf[ModifyHelper])
              Ident(newTermName("ModifyHelper"))
            else
              c.abort(
                c.enclosingPosition,
                s"'$hTpe' is not a valid CommonHelper")),
            newTermName("apply")),
          List(TypeTree(entity.tpe.widen), TypeTree(_tpe))),
        List(attribute, updatable)))
  }

  import scala.collection.mutable
  val cache: mutable.Map[String, Context#Tree] = mutable.Map()

  object OmitNothings
  object IncludeNothings

  def attributeEvidences[Evid: c.WeakTypeTag, A: c.WeakTypeTag, Omit: c.WeakTypeTag]
      (c: Context)(caching: Boolean): c.Expr[Map[String, EvidenceTag]] = {
    import c.mirror._
    import c.universe._

    val omitNothings = c.universe.weakTypeOf[Omit] <:< c.universe.typeOf[OmitNothings.type]
    val aTpe = c.universe.weakTypeOf[A]
    val eTpe = c.universe.weakTypeOf[Evid] match {
      case tr @ ExistentialType(_, TypeRef(pre, _, _)) =>
        tr.asSeenFrom(aTpe.asInstanceOf[TypeRef].pre, pre.typeSymbol)
      case tr @ TypeRef(pre, _, _) =>
        tr.asSeenFrom(aTpe.asInstanceOf[TypeRef].pre, pre.typeSymbol)
    }

    val model = new { val c2: c.type = c } with MacroMetaModel
    import model._

    c.Expr[Map[String, EvidenceTag]](
      Apply(
        Select(Ident(newTermName("Map")), newTermName("apply")), {
          for {
            att <- aTpe.all;
            name = att.toString;
            att_tpe = att.tpe(asf = aTpe);
            if { if(omitNothings) att_tpe.isSomething else true } ;
            tpe = att_tpe.tpe
          } yield {
            val attr = Literal(Constant(name))

            val evid = if (caching) {
              val nTpe = appliedType(eTpe, List(tpe))
              val s = s"$nTpe"
              if (! cache.isDefinedAt(s))
                cache += (s -> c.inferImplicitValue(nTpe))
              cache(s).asInstanceOf[c.Tree]
            } else
              c.inferImplicitValue(appliedType(eTpe, List(tpe)), false)

            if (evid == EmptyTree) {
              c2.abort(
                c2.enclosingPosition,
                s"No evidence found for attribute '$name' of type $tpe")
            }
            Apply(
              Select(Ident(newTermName("Tuple2")), newTermName("apply")),
              List(attr, evid))
          }
        }))
  }

  /**
   * Returns a map with attributes as keys and corresponding evidences as
   * values.
   *
   * Please, refer to [[org.hablapps.updatable.MkAttributeEvidences]] to see
   * the raw code that is being generated by `mkMap`. For invocation and
   * usage examples go to [[org.hablapps.updatable.attributeEvidences]].
   *
   * @tparam Evid the type of the evidence user is looking for
   * @tparam A the type of the entity
   * @param c the macro context
   * @param b the builder that contains the attribute reifications
   * @return the map that contain the evidences for each attribute
   * @see [[org.hablapps.updatable.MkAttributeEvidences]]
   */
  def attributeEvidencesImpl[Evid: c.WeakTypeTag, A: c.WeakTypeTag, Omit: c.WeakTypeTag]
      (c: Context): c.Expr[Map[String, org.hablapps.updatable.EvidenceTag]] = {
    attributeEvidences[Evid, A, Omit](c)(false)
  }

  def fAttributeEvidencesImpl[Evid: c.WeakTypeTag, A: c.WeakTypeTag, Omit: c.WeakTypeTag]
      (c: Context): c.Expr[Map[String, EvidenceTag]] = {
    attributeEvidences[Evid, A, Omit](c)(true)
  }

  def getPosImpl(c: Context): c.Expr[PosInfo] = {
    import c.mirror._
    import c.universe._

    val file = c.enclosingPosition.source.toString.replace("\"", "\\\"")
    val line = c.enclosingPosition.line
    val lineContent = c.enclosingPosition.lineContent.replace("\"", "\\\"")
    val show = c.enclosingPosition.toString.replace("\"", "\\\"")

    c.Expr[PosInfo](c.parse(s"""PosInfo("$file", $line, "$lineContent", "$show")"""))
  }

  def macroAtBuilder(c: Context)
      (annottees: c.Expr[Any]*) 
      (weak: Boolean = false): c.Expr[Any] = {
    import c.universe._
    import c.mirror._

    val classDef @ ClassDef(_, className, _, template) = annottees.head.tree
    val Template(parents, self, body) = template

    lazy val objectConstructor =
      q"def ${nme.CONSTRUCTOR}() = { super.${nme.CONSTRUCTOR}(); () }"

    lazy val typeAlias =
      if (weak)
        q"@weakInnerBuilder type ${c.fresh(className.toTypeName)} = $className"
      else
        q"@innerBuilder type ${c.fresh(className.toTypeName)} = $className"

    val newObjectBody: List[Tree] = List(objectConstructor, typeAlias)

    val newObjectTemplate = Template(
      List(),
      self, 
      newObjectBody)

    val q"$mods object ${_}" = q"object ${newTermName(c.fresh())}"

    val newObjectName = newTermName(className.toString + "Scope")

    val newObjectDef = ModuleDef(
      mods,
      newObjectName, 
      newObjectTemplate)

    lazy val builderAlias =
      q"@org.hablapps.updatable.IAmEntityCompanion def ${className.toTermName} = macro org.hablapps.updatable.Macros.entityToBuilderImpl[$className]"

    val bldImplName =  newTermName(c.fresh("builder"))

    c.Expr[Any](Block(List(classDef, newObjectDef, builderAlias), Literal(Constant(()))))
  }

  def macroAtBuilderImpl(c: Context)(annottees: c.Expr[Any]*) =
    macroAtBuilder(c)(annottees: _*)(false)

  def macroAtWeakBuilderImpl(c: Context)(annottees: c.Expr[Any]*) =
    macroAtBuilder(c)(annottees: _*)(true)

  def macroAtInnerBuilder(c: Context)
      (annottees: c.Expr[Any]*)
      (weak: Boolean = false): c.Expr[Any] = {
    import c.universe._
    import c.mirror._

    val q"type ${_} = $tree" = annottees.head.tree

    val mk = new {
      val c2: c.type = c
    } with MacroMetaModel with MkAtBuilder {
      val entity = c2.typeCheck(q"33.asInstanceOf[$tree]").tpe
    }

    if (weak) mk.weak else mk.apply
  }

  def macroAtInnerBuilderImpl(c: Context)(annottees: c.Expr[Any]*) =
    macroAtInnerBuilder(c)(annottees: _*)(false)

  def macroAtWeakInnerBuilderImpl(c: Context)(annottees: c.Expr[Any]*) =
     macroAtInnerBuilder(c)(annottees: _*)(true)

  def entityToBuilderImpl[A: c.WeakTypeTag](c: Context): c.Expr[Builder[A]] = {
    import c.universe._
    import c.mirror._

    c.Expr[Builder[A]](
      q"""${newTermName(c.universe.weakTypeOf[A].typeSymbol.name.toString + "Scope")}.builder""")
  }

  def macroXtendImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import c.mirror._

    //println("#####>" + weakTypeOf[A])
    println("#####>" + c.enclosingClass)
    c.Expr[Any](q"()")
  }

  trait ExtensionKind
  case object EXTENDS_INTERACTION extends ExtensionKind
  case object EXTENDS_AGENT extends ExtensionKind
  case object EXTENDS_RESOURCE extends ExtensionKind
  case object EXTENDS_SPEECHACT extends ExtensionKind
  case object EXTENDS_JOIN extends ExtensionKind
  case object EXTENDS_ALLOW extends ExtensionKind

  def macroEntityImpl(c: Context)(annottees: c.Expr[Any]*)(parent: ExtensionKind): c.Expr[Any] = { 
    import c.universe._
    import c.mirror._

    val classDef @ ClassDef(mods, className, tparams, template) = annottees.head.tree
    val Template(parents, self, body) = template

    val newParents = List(parent match { 
      case EXTENDS_INTERACTION => tq"Interaction"
      case EXTENDS_AGENT => tq"Agent"
      case EXTENDS_RESOURCE => tq"Resource"
      case EXTENDS_SPEECHACT => tq"SpeechAct"
      case EXTENDS_JOIN => tq"Join"
      case EXTENDS_ALLOW => tq"Allow"
    })
    val newBody = q"type This = $className" :: body
    val newTemplate = Template(newParents, self, newBody)

    val q"$dummy val ${_} = ???" = 
      q"@org.hablapps.updatable.builder val whatever = ???"
    val newModifiers = Modifiers(
      mods.flags, 
      mods.privateWithin, 
      dummy.annotations.head :: mods.annotations)

    val newClassDef = ClassDef(newModifiers, className, tparams, newTemplate)

    c.Expr[Any](Block(List(newClassDef), Literal(Constant(()))))
  }

  def macroInteractionImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    macroEntityImpl(c)(annottees:_*)(EXTENDS_INTERACTION)

  def macroAgentImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    macroEntityImpl(c)(annottees:_*)(EXTENDS_AGENT)

  def macroResourceImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    macroEntityImpl(c)(annottees:_*)(EXTENDS_RESOURCE)

  def macroSpeechactImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    macroEntityImpl(c)(annottees:_*)(EXTENDS_SPEECHACT)

  def macroJoinImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    macroEntityImpl(c)(annottees:_*)(EXTENDS_JOIN)

  def macroAllowImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    macroEntityImpl(c)(annottees:_*)(EXTENDS_ALLOW)
}
