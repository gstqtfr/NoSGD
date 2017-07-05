/*
  LinAlg - Scala Library for Vector and Matrix Types and Operations

  Copyright 2015-2016 Hans-Hermann Bode

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/
package org.garagescience.deeplearning.nosgd.linalg

import scala.collection.generic.CanBuildFrom

/**
 * Vector-specific base trait for builder factories.
 *
 * @param <From> the type of the underlying collection that requests a builder to be created
 * @param <Elem> the element type of the collection to be created
 * @param <To> the type of the collection to be created
 * @since 2.0.0
 * @author h2b
 */
trait VectorCanBuildFrom [-From, Elem, +To] extends CanBuildFrom[From, Elem, To] {

  def apply (from: From): VectorBuilder[Elem, To]

  def apply (): VectorBuilder[Elem, To]

}