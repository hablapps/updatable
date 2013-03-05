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

trait ThisIsAModel { 
  trait E { 
    type E_1
    val e_1: E_1
  }
  implicit val E = weakBuilder[E]
  implicit val x: E = new E { type E_1 = Int; val e_1 = 3 }
}

/* Despite this two models are not used below, they are testing the
 * cake-pattern by their mere definition.
 */
trait ThisIsAnotherModel { this: ThisIsAModel =>
  trait F extends E { 
    type E_1 = Int
  }
  implicit val F = builder[F]
}

@RunWith(classOf[JUnitRunner])
class BuilderTest extends FunSpec 
  with ShouldMatchers 
  with ThisIsAModel
  with ThisIsAnotherModel {

  trait A
  implicit val A = builder[A]

  trait A1 extends A
  implicit val A1 = builder[A1]

  trait A2 extends A { 
    val a2_1: Option[Int]
  }
  implicit val A2 = builder[A2]

  trait B { 
    type B_1Col[_]
    type B_1

    val b_1: B_1Col[B_1]
  }
  implicit val B = weakBuilder[B]

  trait B1 extends B
  implicit val B1 = weakBuilder[B1]

  trait B2 extends B { 
    type B_1Col[x] = List[x]
  }
  implicit val B2 = weakBuilder[B2]

  trait B3 extends B { 
    type B_1Col[x] = List[x]
    type B_1 = Int
  }
  implicit val B3 = builder[B3]
  
  trait B21 extends B2 {
    type B_1 = Int
  }
  implicit val B21 = builder[B21]

  trait C { 
    val c_1: List[String]
  }
  implicit val C = builder[C]

  trait C1 extends C { 
    val c_1: List[String] = Nil
  }
  implicit val C1 = builder[C1]

  trait C11 extends C1
  implicit val C11 = builder[C11]

  trait D { 
    type D_1Col[x] <: Traversable[x]
    type D_1
    type D_2

    val d_1: D_1Col[D_1]
    val d_2: D_2
    val d_3: Option[Double]
    val d_4: String
  }
  implicit val D = weakBuilder[D]

  trait D1 extends D { 
    type D_1Col[x] = Set[x]
    type D_1 = Char
    type D_2 = Int

    val d1_1: Option[Int]
  }
  implicit val D1 = builder[D1]

  trait E { val e_1: Int }
  implicit val NoNamedE = builder[E]

  trait E1 extends E
  implicit val E1 = builder[E1]

  describe("[weak]builder") {

    it("should reify attributes") {
      A2._a2_1.name should be("a2_1")
      B._b_1.name should be("b_1")
      B1._b_1.name should be("b_1")
      B2._b_1.name should be("b_1")
      B3._b_1.name should be("b_1")
      C._c_1.name should be("c_1")
    }

    it("should generate an attribute list") {
      A.attributes map { _.name } should be(List())
      A1.attributes map { _.name } should be(List())
      A2.attributes map { _.name } should be(List("a2_1"))
      B.attributes map { _.name } should be(List("b_1"))
      B1.attributes map { _.name } should be(List("b_1"))
      B2.attributes map { _.name } should be(List("b_1"))
      B3.attributes map { _.name } should be(List("b_1"))
    }

    it("should generate a default apply to create instances") {
      A2().a2_1 should be(default[Option[Int]])
      B3().b_1 should be(default[List[Int]])
      C().c_1 should be(default[List[String]])
    }

    it("should generate an apply (with default params) to create instances") { 
      A2(_a2_1 = Some(3)).a2_1 should be(Some(3))
      B3(_b_1 = List(1, 2, 3)).b_1 should be(List(1, 2, 3))
      C(_c_1 = List("a", "b", "c")).c_1 should be(List("a", "b", "c"))
      val d1 = D1(_d_1 = Set('a', 'b'), _d1_1 = Some(5))
      d1.d_1 should be(Set('a', 'b'))
      d1.d_2 should be(default[Int])
      d1.d_3 should be(default[Option[Double]])
      d1.d_4 should be(default[String])
      d1.d1_1 should be(Some(5))
    }

    it("should generate a get method to retrieve the attribute's values") { 
      A2.get(A2(_a2_1 = Some(3)), A2._a2_1) should be(Some(3))
      B3.get(B3(_b_1 = List(1, 2, 3)), B._b_1) should be(List(1, 2, 3))
      B3.get(B3(_b_1 = List(1, 2, 3)), B3._b_1) should be(List(1, 2, 3))
      C.get(C(_c_1 = List("a", "b", "c")), C._c_1) should be(List("a", "b", "c"))
      D1.get(D1(_d_1 = Set('a')), D1._d_1) should be(Set('a'))
      D1.get(D1(_d_2 = 33), D1._d_2) should be(33)
      D1.get(D1(_d_3 = Option(3.3)), D1._d_3) should be(Option(3.3))
      D1.get(D1(_d_4 = "xyz"), D1._d_4) should be("xyz")
      D1.get(D1(_d1_1 = None), D1._d1_1) should be(None)
    }

    it("should generate an updated method to update an entity") { 
      A2.updated(A2(), A2._a2_1, Some(5)).a2_1 should be(Some(5))
      B3.updated(B3(), B3._b_1, List(1, 2, 3)).b_1 should be(List(1, 2, 3))
      val c = C(List("a"))
      C.updated(c, C._c_1, c.c_1 :+ "b").c_1 should be(List("a", "b"))
      D1.updated(D1(_d_4 = "a"), D._d_4, "b").d_4 should be("b")
    }

    it("should work while having different names for types and builders") {
      E1.updated(E1(), NoNamedE._e_1, 33).e_1 should be(33)
      E1.updated(E1(), E1._e_1, 33).e_1 should be(33)
    }

    it("should allow to invoke the get method at attributes") { 
      A2._a2_1.get(A2.updated(A2(), A2._a2_1, Some(3))) should be(Some(3))
      B3._b_1.get(B3()) should be(List())
    }

    it("should allow to invoke the updated method at attributes") { 
      A2._a2_1.updated(A2(), Option(2)).a2_1 should be(Option(2))
      B3._b_1.updated(B3(), List(1, 2, 3)).b_1 should be(List(1, 2, 3))
    }

    it("should concrete the 'Owner' type at attribute reifications")(pending)

    it("should empower the user to add its own customized code within the builder")(pending)
  }
}
