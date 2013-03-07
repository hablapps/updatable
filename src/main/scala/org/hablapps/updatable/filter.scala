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

package org.hablapps.updatable

import scala.language.higherKinds
import scala.reflect.{ ClassTag, classTag }
import scala.reflect.runtime.universe
import universe.{ typeOf, TypeTag, WeakTypeTag }

trait WithElemClass[C[_]] {
  def classOfElements(i: C[_]): Class[_]
}

class FilterConsWithClass[C1[_]: Modifiable, C2[_]: WithElemClass, V](c: C1[C2[V]]) {
  def filterSubtypesWithElemClass[U <: V: ClassTag] =
    (imodifiable.filter(c) {
      classTag[U].runtimeClass isAssignableFrom classOfElements(_)
    }).asInstanceOf[C1[C2[U]]]
}
