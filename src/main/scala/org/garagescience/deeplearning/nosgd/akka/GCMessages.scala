package org.garagescience.deeplearning.nosgd

import org.apache.spark.ml.linalg.Matrix

sealed trait GCMessages
final case object KickOff       extends GCMessages
final case object FinalWhistle  extends GCMessages
final case object GetUpdateGC   extends GCMessages
final case object AckUpdateGC   extends GCMessages
final case object GetErrorsGC   extends GCMessages
final case object GetMinimumGC  extends GCMessages
final case class  MinimumGC(m: Matrix)
  extends GCMessages
final case class  ErrorsGC(errors: Seq[Double])
  extends GCMessages