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
import scala.language.reflectiveCalls
import language.implicitConversions

@RunWith(classOf[JUnitRunner])
class AtBuilderTest extends FunSpec with ShouldMatchers {

  @builder trait A

  @builder trait A1 extends A

  @builder trait A2 extends A { 
    val a2_1: Option[Int]
  }

  @weakBuilder trait B { 
    type B_1Col[_]
    type B_1

    val b_1: B_1Col[B_1]
  }

  @weakBuilder trait B1 extends B

  @weakBuilder trait B2 extends B { 
    type B_1Col[x] = List[x]
  }

  @builder trait B3 extends B { 
    type B_1Col[x] = List[x]
    type B_1 = Int
  }
  
  @builder trait B21 extends B2 {
    type B_1 = Int
  }

  @builder trait C { 
    val c_1: List[String]
  }

  @builder trait C1 extends C { 
    val c_1: List[String] = Nil
  }

  @builder trait C11 extends C1

  @weakBuilder trait D { 
    type D_1Col[x] <: Traversable[x]
    type D_1
    type D_2

    val d_1: D_1Col[D_1]
    val d_2: D_2
    val d_3: Option[Double]
    val d_4: String
  }

  @builder trait D1 extends D { 
    type D_1Col[x] = Set[x]
    type D_1 = Char
    type D_2 = Int

    val d1_1: Option[Int]
  }

  @builder trait E { val e_1: Int }

  @builder trait E1 extends E

  trait MyEvidence[A] {
    def value: A
  }

  implicit val me1 = new MyEvidence[Int] {
    def value = 33
  }

  @weakBuilder trait G {
    type GType
    implicit def ev: MyEvidence[GType]
  }

  @builder trait G1 extends G {
    type GType = Int
    val g1_1: Option[String]
  }

  implicit val me2 = new MyEvidence[G1] {
    def value = G1().g1_1 += "I am G1"
  }

  @builder trait G2 extends G {
    type GType = Int
    type G2Type = G1
    val g2_1: Option[String]
    implicit def ev2: MyEvidence[G2Type]
  }

  @weakBuilder trait H {
    type New
    implicit def ev1: Builder[New]
  }

  @builder trait H1 extends H {
    type New = G1
  }

  @builder trait J {
    type ContextCol_Default[x] = List[x]
    type Context_Default = Int
    type ContextCol[_]
    type Context

    val context3: Context
    val context2: List[Context]
    val context1: ContextCol[String]
    val context4: ContextCol[Context]
  }

  @builder trait J1 extends J

  @builder trait K {
    type ContextCol_Default[x] = List[x]
    type ContextCol[_]
    type Context = Int // without default

    val context: ContextCol[Context]
  }

  @builder trait K1 extends K {
    type ContextCol[x] = Set[x]
  }

  @weakBuilder trait L {
    type Context
    type Context_Default = Int
    
    val l1: Context
  }

  @weakBuilder trait L1 extends L {
    type Context <: Int
  }

  @builder trait L11 extends L1

  trait HK[_]

  @builder trait M { 
    val a1: Option[HK[Int]]
  }

  @builder trait N { 
    val n1: Option[{ val whatever: Int }]
  }

  @weakBuilder trait O { 
    type T_Default = Nothing
    type T

    val a1: Option[T]
  }

  @builder trait O1 extends O {
    val a2: Option[T]
  }

  @weakBuilder trait P { 
    type T_Default = Nothing
    type T
  }

  @builder trait P1 extends P {
    type U = T

    /* TODO: The problem here is that a1's type signature doesn't contain T.
     * Therefore, the type substitution doesn't work properly, because the T
     * symbol is not found inside the type. What if we use a U_Default? The
     * thing is that U is not abstract, I mean, it is defined to T. The lack of
     * homogeneity between 'abxtract' (abstract attributes) and 'abstractTpes'
     * (types which are not declared) leads to a wrong way to substitute the
     * defaults.
     */
    // val a1: Option[U]

    /* So, the only way to workaround this is to avoid using alias, by setting
     * the type with defaults directly in the signature.
     */
    val a1: Option[T]
  }

  @builder trait Interaction { 
    val a1: Int
  }

  @interaction trait Q

  describe("Temporal @interaction") {
    assert((Q().a1 := 33).a1 == 33)
  }

  describe("[weak]builder") {

    it("should reify attributes") {
      A2._a2_1.toString should be("a2_1")
      B._b_1.toString should be("b_1")
      B1._b_1.toString should be("b_1")
      B2._b_1.toString should be("b_1")
      B3._b_1.toString should be("b_1")
      C._c_1.toString should be("c_1")
    }

    it("should generate an attribute list") {
      A.attributes map { _.toString } should be(List())
      A1.attributes map { _.toString } should be(List())
      A2.attributes map { _.toString } should be(List("a2_1"))
      B.attributes map { _.toString } should be(List("b_1"))
      B1.attributes map { _.toString } should be(List("b_1"))
      B2.attributes map { _.toString } should be(List("b_1"))
      B3.attributes map { _.toString } should be(List("b_1"))
    }

    it("should generate a default apply to create instances") {
      A2().a2_1 should be(default[Option[Int]])
      B3().b_1 should be(default[List[Int]])
      C().c_1 should be(default[List[String]])
    }

    // it("should generate an apply (with default params) to create instances") { 
    //   A2(_a2_1 = Some(3)).a2_1 should be(Some(3))
    //   B3(_b_1 = List(1, 2, 3)).b_1 should be(List(1, 2, 3))
    //   C(_c_1 = List("a", "b", "c")).c_1 should be(List("a", "b", "c"))
    //   val d1 = D1(_d_1 = Set('a', 'b'), _d1_1 = Some(5))
    //   d1.d_1 should be(Set('a', 'b'))
    //   d1.d_2 should be(default[Int])
    //   d1.d_3 should be(default[Option[Double]])
    //   d1.d_4 should be(default[String])
    //   d1.d1_1 should be(Some(5))
    // }

    it("should generate a get method to retrieve the attribute's values") { 
      A2.get(A2(_a2_1 = Some(3)), A2._a2_1) should be(Some(3))
      B3.get(B3(_b_1 = List(1, 2, 3)), B._b_1) should be(List(1, 2, 3))
      B3.get(B3(_b_1 = List(1, 2, 3)), B3._b_1) should be(List(1, 2, 3))
      C.get(C(_c_1 = List("a", "b", "c")), C._c_1) should be(List("a", "b", "c"))
      D1.get(D1().d_1 += 'a', D1._d_1) should be(Set('a'))
      D1.get(D1().d_2 := 33, D1._d_2) should be(33)
      D1.get(D1().d_3 += 3.3, D1._d_3) should be(Option(3.3))
      D1.get(D1().d_4 := "xyz", D1._d_4) should be("xyz")
      D1.get(D1().d1_1 := None, D1._d1_1) should be(None)
    }

    it("should generate an updated method to update an entity") { 
      A2.updated(A2(), A2._a2_1, Some(5)).a2_1 should be(Some(5))
      B3.updated(B3(), B3._b_1, List(1, 2, 3)).b_1 should be(List(1, 2, 3))
      val c = C(List("a"))
      C.updated(c, C._c_1, c.c_1 :+ "b").c_1 should be(List("a", "b"))
      D1.updated(D1().d_4 := "a", D._d_4, "b").d_4 should be("b")
    }

    it("should allow to invoke the get method at attributes") { 
      A2._a2_1.get(A2.updated(A2(), A2._a2_1, Some(3))) should be(Some(3))
      B3._b_1.get(B3()) should be(List())
    }

    it("should allow to invoke the updated method at attributes") { 
      A2._a2_1.updated(A2(), Option(2)).a2_1 should be(Option(2))
      B3._b_1.updated(B3(), List(1, 2, 3)).b_1 should be(List(1, 2, 3))
    }

    it("should allow getting the required evidences") {
      G1().ev.value should be(33)
      G2().ev.value should be(33)
      (G2().g2_1 += "hi!").ev.value should be(33)
      G2().ev2.value.g1_1.get should be("I am G1")
      H1().ev1 should be(G1)
    }

    it("should concrete the 'Owner' type at attribute reifications")(pending)

    it("should empower the user to add its own customized code within the builder")(pending)
  }
}
