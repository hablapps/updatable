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

@RunWith(classOf[JUnitRunner])
class SafeUpdateTest extends FunSpec with ShouldMatchers {

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

  trait F { 
    val f1: Option[Int]
  }

  trait G { 
    val g1: Array[Int]
  }

  describe("Safe Update Methods") {

    it("should update an attribute whole value (container + element)") { 
      (A2().a2_1 := Some(5)).a2_1 should be(Some(5))
      (B3(_b_1 = List(1)).b_1 := List(1, 2, 3)).b_1 should be(List(1, 2, 3))
      (C().c_1 := List("a", "b", "c")).c_1 should be(List("a", "b", "c"))
      (D1(
  // just to exercise updatable printing...
  _d_1 = Set('a'),
  _d_2 = 3,
  _d_3 = Option(1.0),
  _d_4 = "",
  _d1_1 = Some(4)
      ).d_4 := "xyz").d_4 should be("xyz")
      (E1().e_1 := default[Int]).e_1 should be(default[Int])
    }

    it("should modify an attribute value by adding items to the container") {
      (A2().a2_1 += 3).a2_1 should be(Option(3))
      (A2(Option(1)).a2_1 += 3).a2_1 should be(Option(3))
      (B3().b_1 += 1).b_1 should be(List(1))
      (B3(_b_1 = List(1)).b_1 += 2).b_1 should be(List(1, 2))
      (D1().d_3 += 3.3).d_3 should be(Option(3.3))
      (D1().d_1 += '1').d_1 should be(Set('1'))
    }

    it("should modify an attribute value by removing items from the container") { 
      (A2(_a2_1 = Option(5)).a2_1 -= 3).a2_1 should be(Option(5))
      (A2(_a2_1 = Option(5)).a2_1 -= 5).a2_1 should be(None)
      (B3(_b_1 = List(3)).b_1 -= 1).b_1 should be(List(3))
      (B3(_b_1 = List(1)).b_1 -= 1).b_1 should be(List())
      (B3(_b_1 = List(1, 2, 1, 2, 1)).b_1 -= 1).b_1 should be(List(2, 2))
      (D1().d_3 -= 3.3).d_3 should be(None)
    }

    it("should chain several modifications") { 
      (((B3(_b_1 = List(2)).b_1 += 1).b_1 += 3).b_1 += 4).b_1 should be(List(2, 1, 3, 4))
      ((A2().a2_1 += 5).a2_1 -= 5).a2_1 should be(None)
      ((B3().b_1 := List(1, 2, 3, 4, 5)).b_1 -= 3).b_1 should be(List(1, 2, 4, 5))
    }

    // not even a runtime test
    it("should arise a warning when a non-final attribute is being modified") { 
      def foo[R <: F : Builder](a: R) = a.f1 := None
      def bar[R <: G : Builder](a: R) = a.g1 := Array()
    }
  }
}
