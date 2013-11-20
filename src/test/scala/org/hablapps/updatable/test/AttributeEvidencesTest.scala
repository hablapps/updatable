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
import Macros.OmitNothings
import org.junit.runner.RunWith
import scala.language.higherKinds
import scala.language.reflectiveCalls
import language.experimental.macros

@RunWith(classOf[JUnitRunner])
class AttributeEvidencesTest extends FunSpec with ShouldMatchers {

  trait TestEvidence[T] extends EvidenceTag { 
    def id: Int
    override def equals(other: Any) = other match { 
      case t: TestEvidence[_] => id == t.id
    }
  }

  implicit def listEvidence[T]: TestEvidence[List[T]] =
    new TestEvidence[List[T]] { def id = 1 }

  implicit def intEvidence: TestEvidence[Int] =
    new TestEvidence[Int] { def id = 2 }

  implicit def doubleEvidence: TestEvidence[Double] = 
    new TestEvidence[Double] { def id = 2 }

  implicit def optionEvidence[T]: TestEvidence[Option[T]] =
    new TestEvidence[Option[T]] { def id = 3 }

  implicit def setEvidence[T]: TestEvidence[Set[T]] =
    new TestEvidence[Set[T]] { def id = 4 }

  implicit def traversableEvidence[T]: TestEvidence[Traversable[T]] =
    new TestEvidence[Traversable[T]] { def id = 5 }

  @builder trait A {
    val a_1: List[String]
    val a_2: Int
    val a_3: Double
    val a_4: Option[Int]
    val a_5: Set[Char]
  }

  @weakBuilder trait B { 
    type B_1Col[_]
    type B_1

    val b_1: B_1Col[B_1]
    val b_2: List[B_1]
  }

  @builder trait B1 extends B { 
    type B_1Col[x] = Traversable[x]
    type B_1 = Int

    val b_3: Option[B1]
  }

  describe("attributeEvidences[E[_], T](b: Builder[T])") {

    it ("should find the attribute evidences given the simplest builder") {
      val m = attributeEvidences[TestEvidence[_], A, OmitNothings.type]
      m("a_1") should be(listEvidence)
      m("a_2") should be(intEvidence)
      m("a_3") should be(doubleEvidence)
      m("a_4") should be(optionEvidence)
      m("a_5") should be(setEvidence)
    }

    it("should find the attribute evidences given a more complex builder") { 
      val m = attributeEvidences[TestEvidence[_], B1, OmitNothings.type]
      m("b_1") should be(traversableEvidence)
      m("b_2") should be(listEvidence)
      m("b_3") should be(optionEvidence)
    }
  }

  @builder trait C {
    type C1Col_Default[x] = List[x]
    type C1Col[_]
    type C2_Default = Int
    type C2

    val c1: C1Col[Int]
    val c2: C2
  }

  describe("defaultAttributeEvidences") {

    it ("should find the attribute evidences while default types are involved") {
      val m = attributeEvidences[TestEvidence[_], C, OmitNothings.type]
      m("c1") should be(listEvidence)
      m("c2") should be(intEvidence)
    }
  }
}
