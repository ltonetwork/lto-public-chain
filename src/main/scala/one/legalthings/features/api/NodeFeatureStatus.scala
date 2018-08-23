package one.legalthings.features.api

sealed trait NodeFeatureStatus

object NodeFeatureStatus {
  case object NotImplemented extends NodeFeatureStatus
  case object Implemented    extends NodeFeatureStatus
  case object Voted          extends NodeFeatureStatus
}
