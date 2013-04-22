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

package org.hablapps.updatable.test

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.ShouldMatchers
import org.hablapps.updatable._
import org.junit.runner.RunWith
import scala.language.higherKinds

trait ExternalModel {
  type AbsCol[_]
  type Abs

  trait D { 
    type D_1

    val d_1: D_1
    val d_2: AbsCol[D_1]
    val d_3: Option[D_1]
    val d_4: Option[AbsCol[D_1]]
    val d_5: Option[Option[Option[Option[Option[D_1]]]]]
  }

  trait E { 
    val e_1: Abs
    val e_2: AbsCol[Int]
    val e_3: Option[Abs]
  }
}

/** Tests the MetaModel.
  *
  * This tests the metaModel at three different levels: firstly it exercises the
  * `Type` functionality; secondly it exercises the `Attribute` methods; and
  * finally, it exercises the `AttributeType` ones.
  *
  * @see [[org.hablapps.updatable.MetaModelAPI]]
  */
@RunWith(classOf[JUnitRunner])
class MetaModelTest extends FunSpec 
  with ShouldMatchers 
  with RuntimeMetaModel 
  with ExternalModel {
  import universe._

  trait A

  trait A1 extends A

  trait A2 extends A { 
    val a2_1: Option[Int]
  }

  trait B { 
    type B_1Col[_]
    type B_1

    val b_1: B_1Col[B_1]
  }

  trait B1 extends B

  trait B2 extends B { 
    type B_1Col[x] = List[x]
  }

  trait B3 extends B { 
    type B_1Col[x] = List[x]
    type B_1 = Int
  }

  trait C { 
    val c_1: List[String]
  }

  trait C1 extends C { 
    val c_1: List[String] = Nil
  }

  trait C11 extends C1

  trait F { 
    val f_1: Id[String]
    val f_2: String
  }

  trait G { 
    type G_1
    val g_1: G_1
  }

  trait G1 extends G { 
    type G_1 = Int { def dummy: Int } // tests refined types
  }

  import scala.collection.mutable. { HashSet, LinkedHashSet }

  trait J {
    type J_3
    type J_4Col[_]
    type J_4

    val j_1: Option[Int]
    val j_2: String
    val j_3: J_3
    val j_4: J_4Col[J_4]
  }

  trait J1 extends J { 
    val j_1: Some[Int]
  }

  trait J2 extends J { 
    type J_3 = Int
    override type J_4Col[x] = Array[x]
    type J_4 = Int
  }

  trait J3 extends J { 
    type J_3 = Int
    type J_4Col[x] = List[x]
    type J_4 = Option[Int]
  }

  trait K {
    val k_1: Int @default("5")
    val k_2: String @default("\"\"")
    val k_3: Double @default("1.0")
    val k_4: List[Int] @default("List[Int](1, 2, 3)")
  }

  describe("Runtime MetaModel") {

    val _a = typeOf[A] // ask scalatest why 'a' is invalid
    val a1 = typeOf[A1]
    val a2 = typeOf[A2]
    val b = typeOf[B]
    val b1 = typeOf[B1]
    val b2 = typeOf[B2]
    val b3 = typeOf[B3]
    val c = typeOf[C]
    val c1 = typeOf[C1]
    val c11 = typeOf[C11]
    val d = typeOf[D]
    val e = typeOf[E]
    val f = typeOf[F]
    val g = typeOf[G]
    val g1 = typeOf[G1]
    val j = typeOf[J]
    val j1 = typeOf[J1]
    val j2 = typeOf[J2]
    val j3 = typeOf[J3]
    val k = typeOf[K]

    // Types

    it("should know the type name") {
      _a.name should be("A")
      a1.name should be("A1")
      a2.name should be("A2")
      b.name should be("B")
      b1.name should be("B1")
      b2.name should be("B2")
      b3.name should be("B3")
      c.name should be("C")
      c1.name should be("C1")
      c11.name should be("C11")
    }

    it("should get all the attributes") { 
      _a.all map { _.name } should be(List())
      a1.all map { _.name } should be(List())
      a2.all map { _.name } should be(List("a2_1"))
      b.all map { _.name } should be(List("b_1"))
      b1.all map { _.name } should be(List("b_1"))
      b2.all map { _.name } should be(List("b_1"))
      b3.all map { _.name } should be(List("b_1"))
      c.all map { _.name } should be(List("c_1"))
      c1.all map { _.name } should be(List())
      c11.all map { _.name } should be(List())
    }

    it("should get only the declared attributes") { 
      _a.declared map { _.name } should be(List())
      a1.declared map { _.name } should be(List())
      a2.declared map { _.name } should be(List("a2_1"))
      b.declared map { _.name } should be(List("b_1"))
      b1.declared map { _.name } should be(List())
      b2.declared map { _.name } should be(List())
      b3.declared map { _.name } should be(List())
      c.declared map { _.name } should be(List("c_1"))
      c1.declared map { _.name } should be(List())
      c11.declared map { _.name } should be(List())
    }
    
    it("should get only the inherited attributes") { 
      _a.inherited map { _.name } should be(List())
      a1.inherited map { _.name } should be(List())
      a2.inherited map { _.name } should be(List())
      b.inherited map { _.name } should be(List())
      b1.inherited map { _.name } should be(List("b_1"))
      b2.inherited map { _.name } should be(List("b_1"))
      b3.inherited map { _.name } should be(List("b_1"))
      c.inherited map { _.name } should be(List())
      c1.inherited map { _.name } should be(List())
      c11.inherited map { _.name } should be(List())
    }

    it("should get only the abstract attributes") {
      _a.abxtract map { _.name } should be(List())
      a1.abxtract map { _.name } should be(List())
      a2.abxtract map { _.name } should be(List())
      b.abxtract map { _.name } should be(List("b_1"))
      b1.abxtract map { _.name } should be(List("b_1"))
      b2.abxtract map { _.name } should be(List("b_1"))
      b3.abxtract map { _.name } should be(List())
      c.abxtract map { _.name } should be(List())
      c1.abxtract map { _.name } should be(List())
      c11.abxtract map { _.name } should be(List())
    }

    it("should get only the final attributes") {
      j.fynal map {  _.name } should be(List("j_2"))
      // j1.fynal map {  _.name } should be(List("j_1", "j_2"))
      j2.fynal map {  _.name } should be(List("j_2", "j_3", "j_4"))
      j3.fynal map {  _.name } should be(List("j_2", "j_3"))
    }

    it("should consider only local types to check if an attribute is abstract") {
      d.abxtract map { _.name } should be(List("d_1", "d_2", "d_3", "d_4", "d_5"))
      e.abxtract map { _.name } should be(List())
    }

    it("should get only the concreted attributes") {
      _a.concreted map { _.name } should be(List())
      a1.concreted map { _.name } should be(List())
      a2.concreted map { _.name } should be(List("a2_1"))
      b.concreted map { _.name } should be(List())
      b1.concreted map { _.name } should be(List())
      b2.concreted map { _.name } should be(List())
      b3.concreted map { _.name } should be(List("b_1"))
      c.concreted map { _.name } should be(List("c_1"))
      c1.concreted map { _.name } should be(List())
      c11.concreted map { _.name } should be(List())
    }

    it("should know if the entity is either abstract or concrete") {
      _a.isAbstract should be(false) 
      a1.isAbstract should be(false) 
      a2.isAbstract should be(false) 
      b.isAbstract should be(true) 
      b1.isAbstract should be(true) 
      b2.isAbstract should be(true) 
      b3.isAbstract should be(false)
      c.isAbstract should be(false)
      c1.isAbstract should be(false)
      c11.isAbstract should be(false)

      _a.isConcrete should be(true)
      a1.isConcrete should be(true)
      a2.isConcrete should be(true)
      b.isConcrete should be(false)
      b1.isConcrete should be(false)
      b2.isConcrete should be(false)
      b3.isConcrete should be(true)
      c.isConcrete should be(true)
      c1.isConcrete should be(true)
      c11.isConcrete should be(true)
    }

    it("should know if the entity is empty") { 
      _a.isEmpty should be(true)
      a1.isEmpty should be(true)
      a2.isEmpty should be(false)
      b.isEmpty should be(false)
      b1.isEmpty should be(false)
      b2.isEmpty should be(false)
      b3.isEmpty should be(false)
      c.isEmpty should be(false)
      c1.isEmpty should be(false)
      c11.isEmpty should be(false)
    }

    it("should keep a reference to the main base class") { 
      b.base should be (None)
      b1.base.get.name should be (b.name)
      b2.base.get.name should be (b.name)
      b3.base.get.name should be (b.name)
      c.base should be (None)
      c1.base.get.name should be (c.name)
      c11.base.get.name should be (c1.name)
    }

    it("should know where is the associated builder")(pending)

    // Attributes

    val a2_1 = a2.declared(0)
    val b_1 = b.declared(0)
    val c_1 = c.declared(0)
    val f_1 = f.declared(1)
    val f_2 = f.declared(0)
    val g_1 = g.declared(0)
    val k_1 = k.declared(3)
    val k_2 = k.declared(2)
    val k_3 = k.declared(1)
    val k_4 = k.declared(0)

    it("should know an attribute's name") { 
      a2_1.name should be("a2_1")
      b_1.name should be("b_1")
      c_1.name should be("c_1")
    }

    it("should know an attribute's owner") { 
      a2_1.owner.name should be("A2")
      b_1.owner.name should be("B")
      c_1.owner.name should be("C")
    }

    it("should know if an attribute is either abstract or concrete") { 
      a2_1.isAbstract(a2) should be(false)
      b_1.isAbstract(b) should be(true)
      b_1.isAbstract(b1) should be(true)
      b_1.isAbstract(b2) should be(true)
      b_1.isAbstract(b3) should be(false)
      c_1.isAbstract(c) should be(false)
 
      a2_1.isConcrete(a2) should be(true)
      b_1.isConcrete(b) should be(false)
      b_1.isConcrete(b1) should be(false)
      b_1.isConcrete(b2) should be(false)
      b_1.isConcrete(b3) should be(true)
      c_1.isConcrete(c) should be(true)
    }

    it("should know if an attribute is either declared or inherited") {
      a2_1.isDeclared(a2) should be(true)
      b_1.isDeclared(b) should be(true)
      b_1.isDeclared(b1) should be(false)
      b_1.isDeclared(b2) should be(false)
      b_1.isDeclared(b3) should be(false)
      c_1.isDeclared(c) should be(true)

      a2_1.isInherited(a2) should be(false)
      b_1.isInherited(b) should be(false)
      b_1.isInherited(b1) should be(true)
      b_1.isInherited(b2) should be(true)
      b_1.isInherited(b3) should be(true)
      c_1.isInherited(c) should be(false)
    }

    it("should know if an attribute is either final or not") {
      // we need to redesign 'final' issues
    }

    it("should know if an attribute is either deferred or undeferred") { 
      a2_1.isDeferred(a2) should be(true)
      b_1.isDeferred(b) should be(true)
      b_1.isDeferred(b1) should be(true)
      b_1.isDeferred(b2) should be(true)
      b_1.isDeferred(b3) should be(true)
      c_1.isDeferred(c) should be(true)

      a2_1.isUndeferred(a2) should be(false)
      b_1.isUndeferred(b) should be(false)
      b_1.isUndeferred(b1) should be(false)
      b_1.isUndeferred(b2) should be(false)
      b_1.isUndeferred(b3) should be(false)
      c_1.isUndeferred(c) should be(false)
    }

    it("should extract default values from annotations") {
      k_1.default.get should be (5)
      k_2.default.get should be ("")
      k_3.default.get should be (1.0)
      k_4.default.get should be(List(1, 2, 3))
    }

    // Attribute Types

    val a2_1_asfA2 = a2_1.tpe(a2)
    val b_1_asfB = b_1.tpe(b)
    val b_1_asfB1 = b_1.tpe(b1)
    val b_1_asfB2 = b_1.tpe(b2)
    val b_1_asfB3 = b_1.tpe(b3)
    val c_1_asfC = c_1.tpe(c)
    val f_1_asfF = f_1.tpe(f)
    val f_2_asfF = f_2.tpe(f)
    val g_1_asfG1 = g_1.tpe(g1)
    val k_1_asfK = k_1.tpe(k)

    it("should know if an attribute's type is either abstract or concrete") { 
      a2_1_asfA2.isAbstract(a2) should be(false)
      b_1_asfB.isAbstract(b) should be(true)
      b_1_asfB1.isAbstract(b1) should be(true)
      b_1_asfB2.isAbstract(b2) should be(true)
      b_1_asfB3.isAbstract(b3) should be(false)
      c_1_asfC.isAbstract(c) should be(false)
      
      a2_1_asfA2.isConcrete(a2) should be(true)
      b_1_asfB.isConcrete(b) should be(false)
      b_1_asfB1.isConcrete(b1) should be(false)
      b_1_asfB2.isConcrete(b2) should be(false)
      b_1_asfB3.isConcrete(b3) should be(true)
      c_1_asfC.isConcrete(c) should be(true)
    }

    it ("should print the right attribute type while 'Id' is explicitly set") { 
      f_1_asfF.toString should be("Id[String]")
      f_2_asfF.toString should be("Id[String]")
    }

    it("should identify the Id types") {
      a2_1_asfA2.isId should be(false)
      b_1_asfB3.isId should be(false)
      f_1_asfF.isId should be(true)
      f_2_asfF.isId should be(true)
    }

    it("should print a valid name for refined types") {
      g_1_asfG1.toString should be("Id[Int{def dummy: Int}]")
    }
  }
}
