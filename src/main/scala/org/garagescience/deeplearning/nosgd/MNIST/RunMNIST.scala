package org.garagescience.deeplearning.nosgd.MNIST

import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg._
import breeze.numerics.sigmoid
import breeze.stats.distributions.Rand
import scala.util.Random

object RunMNIST extends App {

  val net = new SGDNetwork(List(784, 30, 10))

  val data = new MNISTLoader().load()

  net.minimiseError(data._1, 30, 10, 3.0, Option(data._2))
}
