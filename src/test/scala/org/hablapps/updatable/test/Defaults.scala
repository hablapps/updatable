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

class Defaults extends FunSpec with Matchers with BeforeAndAfter {

  it("higher kinds") {
    higherDefault[Set,Int] should be(Set[Int]())
    higherDefault[Option,Int] should be(None)
    higherDefault[List,Int] should be(List[Int]())
    higherDefault[Traversable,Int] should be(Traversable[Int]())
  }

}
