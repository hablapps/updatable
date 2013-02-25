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
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.hablapps.updatable._

@RunWith(classOf[JUnitRunner])
class ApplyNull extends FunSpec with ShouldMatchers with BeforeAndAfter {

  trait A {
    val a1: Option[Int]
    val a2: List[Int]
    val a3: String
    val a4: Boolean
    val a5: Int
  }
  implicit val A = builder[A]

  it("AnyRefs") {
    val a = A.applyNull()
    a should not be (null)
    a.a1 should be(null)
    a.a2 should be(null)
    a.a3 should be(null)
    a.a4 should be(false)
    a.a5 should be(0)
  }
}
