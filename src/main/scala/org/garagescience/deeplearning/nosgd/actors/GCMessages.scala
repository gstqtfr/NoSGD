package org.garagescience.deeplearning.nosgd.actors

import org.garagescience.deeplearning.nosgd.mlp.data.DataSet

sealed trait GCMessages
final case object KickOff       extends GCMessages
final case object FinalWhistle  extends GCMessages
final case object GetUpdateGC   extends GCMessages
final case object AckUpdateGC   extends GCMessages
final case object GetErrorsGC   extends GCMessages
final case object GetMinimumGC  extends GCMessages

final case class DataGC(data: Iterator[DataSet])
  extends GCMessages

final case object DataGCAck     extends GCMessages

// TODO: we *might* need to add e.g. iterations here ...
final case class  MinimumGC(xs: Array[Double])
  extends GCMessages

// TODO: need rethink this also ... maybe ...
final case class  ErrorsGC(errors: Array[Double])
  extends GCMessages


// thee allow us to keep track of where we are
final case class ThisDataGC(iterations: Int,
                            data: DataSet) extends GCMessages

final case class  TheseErrorsGC(iterations: Int,
                                errors: Array[Double]) extends GCMessages

