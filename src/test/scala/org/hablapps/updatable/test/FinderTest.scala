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

import org.scalatest.FunSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.Matchers
import org.hablapps.updatable._
import language.experimental.macros

trait StateA {
  @builder trait A {
    val a1: Int
  }
}

trait StateB {
  @builder trait B {
    val b1: String
  }
}

trait StateC {
  @builder trait C {
    val c1: List[Int]
  }
}

object Sys0 extends StateA

object Sys1 extends StateA with StateB

object Sys2 extends StateA with StateB with StateC

class FinderTest extends FunSpec with Matchers {
  import model.universe._

  val m0 = finder[Sys0.type]
  val m1 = finder[Sys1.type]
  val m2 = finder[Sys2.type]

  describe("finder") {

    it("should infer the model's name") {
      m0.name should be("Sys0")
      m1.name should be("Sys1")
      m2.name should be("Sys2")
    }

    it("should find all the types hosted in a system") {
      m0.whole should be(List(typeOf[Sys0.A]))
      m1.whole should be(List(typeOf[Sys1.A], typeOf[Sys1.B]))
      m2.whole should be(List(typeOf[Sys2.A], typeOf[Sys2.B], typeOf[Sys2.C]))
    }

    it("should know the number of entities existing in the model") {
      m0.size should be(1)
      m1.size should be(2)
      m2.size should be(3)
    }
  }
}
