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

  trait A {
    val a_1: List[String]
    val a_2: Int
    val a_3: Double
    val a_4: Option[Int]
    val a_5: Set[Char]
  }
  val A = builder[A]

  trait B { 
    type B_1Col[_]
    type B_1

    val b_1: B_1Col[B_1]
    val b_2: List[B_1]
  }
  implicit val B = weakBuilder[B]

  trait B1 extends B { 
    type B_1Col[x] = Traversable[x]
    type B_1 = Int

    val b_3: Option[B1]
  }
  implicit val B1 = builder[B1]

  describe("attributeEvidences[E[_], T](b: Builder[T])") {

    it ("should find the attribute evidences given the simplest builder") {
      val m = attributeEvidences[TestEvidence[_], A](A)
      m(A._a_1) should be(listEvidence)
      m(A._a_2) should be(intEvidence)
      m(A._a_3) should be(doubleEvidence)
      m(A._a_4) should be(optionEvidence)
      m(A._a_5) should be(setEvidence)
    }

    it("should find the attribute evidences given a more complex builder") { 
      val m = attributeEvidences[TestEvidence[_], B1](B1)
      m(B1._b_1) should be(traversableEvidence)
      m(B1._b_2) should be(listEvidence)
      m(B1._b_3) should be(optionEvidence)
    }
  }
}
