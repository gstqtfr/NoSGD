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

import scala.collection.mutable.Map
import scala.reflect.{ ClassTag, classTag }

//import de.h2b.scala.lib.math.linalg.Vector.At
import org.garagescience.deeplearning.nosgd.linalg.Vector.At

/**
 * @author h2b
 */
trait VectorFactory [E] {

  val zero: E
  val one: E

  def create (startIndex: Int, elems: Seq[E]): Vector[E]

}

/**
 * Creates vector instances by concrete factories depending on the derived
 * element type..
 * <p/>
 * Currently, all standard numeric element types are supported. All others will
 * cause an exception to be thrown as long as no additional mappings are
 * registered using the {@code register} facility.
 *
 * @author h2b
 */
object VectorFactory {

	private class FactoryMap {

		private val map = Map[Class[_ <: Any], VectorFactory[_ <: Any]]()

		def put [E] (key: Class[E], value: VectorFactory[E]) = map.put(key, value)

		def get [E] (key: Class[E]) = map.get(key).asInstanceOf[Option[VectorFactory[E]]]

	}

  private val factories = new FactoryMap()

  register(classOf[Double], DoubleVectorFactory)
  register(classOf[Float], NumericFloatVectorFactory)
  register(classOf[Long], NumericLongVectorFactory)
  register(classOf[Int], IntVectorFactory)
  register(classOf[Short], NumericShortVectorFactory)
  register(classOf[Byte], NumericByteVectorFactory)
  register(classOf[Char], NumericCharVectorFactory)

  def register [E] (forType: Class[E], factory: VectorFactory[E]): Unit =
    factories.put(forType, factory)

  /**
   * @throws UnsupportedOperationException if the element type is not supported
   */
  def apply [E : ClassTag] (low: At, seq: Seq[E]) = {
    val elemType = classTag[E].runtimeClass
    factories.get(elemType) match {
      case Some(factory) =>
        factory.asInstanceOf[VectorFactory[E]].create(low.index, seq)
      case None =>
        throw new UnsupportedOperationException("element type not supported: " + elemType)
    }
  }

  /**
   * @throws UnsupportedOperationException if the element type is not supported
   */
  def zero [E : ClassTag]: E = {
    val elemType = classTag[E].runtimeClass
    factories.get(elemType) match {
      case Some(factory) =>
        factory.asInstanceOf[VectorFactory[E]].zero
      case None =>
        throw new UnsupportedOperationException("element type not supported: " + elemType)
    }
  }

  /**
   * @throws UnsupportedOperationException if the element type is not supported
   */
  def one [E : ClassTag]: E = {
    val elemType = classTag[E].runtimeClass
    factories.get(elemType) match {
      case Some(factory) =>
        factory.asInstanceOf[VectorFactory[E]].one
      case None =>
        throw new UnsupportedOperationException("element type not supported: " + elemType)
    }
  }

}
