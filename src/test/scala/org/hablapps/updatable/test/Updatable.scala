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

class UpdatableTest extends FunSpec with Matchers with BeforeAndAfter {


  @builder trait A {
    val a1: Option[Int]
    val a2: List[Int]
    val a3: String
    val a4: Boolean
    val a5: Int
  }

  val a_value: Updatable[A] = A()

  it("AnyRefs") {
    val Updatable(value) = a_value
  }


  it("in for-comprehensions") {
    for {
      Updatable(value) <- Some(a_value)
    } yield ()

    val Some(value: A) = Some(a_value.value)
    a_value.a1 shouldBe None
  }

}
