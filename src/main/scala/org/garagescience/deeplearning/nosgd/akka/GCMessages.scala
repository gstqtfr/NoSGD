package org.garagescience.deeplearning.nosgd

sealed trait GCMessages
final case object KickOff      extends GCMessages
final case object FinalWhistle extends GCMessages
final case object GetUpdateGC  extends GCMessages
final case object AckUpdateGC  extends GCMessages
final case object GetErrorsGC  extends GCMessages
final case class  ErrorsGC(errors: Seq[Double])
  extends GCMessages