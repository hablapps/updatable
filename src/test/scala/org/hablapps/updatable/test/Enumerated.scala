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

import scalaz.{Success, Failure}
import org.scalatest.FunSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.Matchers
import org.hablapps.updatable._
import scala.language.higherKinds
import scala.language.reflectiveCalls
import language.experimental.macros

class Enumerated extends FunSpec with Matchers {

  object System{
    trait T extends org.hablapps.updatable.Enumerated
    object t1 extends T
    object t2 extends T

    trait S extends org.hablapps.updatable.Enumerated
    object s1 extends S
    object s2 extends S
  }

  trait R extends org.hablapps.updatable.Enumerated

  describe("Enumerated values") {

    it("can be recovered if they exist and are of the proper type") {
      getEnumeratedVal[System.T]("t1",System) should be(Success(System.t1))
    }

    it("can't be retrieved if they don't exist"){
      getEnumeratedVal[System.T]("tttttt",System) should be(
        Failure(EnumeratedException[System.T]("tttttt"))
      )
    }

    it("can't be retrieved if their types don't match"){
      getEnumeratedVal[System.T]("s1",System) should be(
        Failure(EnumeratedException[System.T]("s1"))
      )
    }

    it("can't be retrieved if their types don't match (exogenous type)"){
      getEnumeratedVal[R]("s1",System) should be(
        Failure(EnumeratedException[R]("s1"))
      )
    }
  }
}
