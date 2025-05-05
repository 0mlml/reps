package com.reps.models
import scala.collection.immutable._
import java.time.LocalDateTime

enum AlertType:
  case SystemAlert, LowOutput, SystemError, EquipmentMalfunction

case class Alert(
  alertType: AlertType,
  source: EnergyType,
  message: String,
  timestamp: LocalDateTime
)

object Alert:
  def create(source: EnergyType, message: String, timestamp: LocalDateTime): Alert =
    Alert(
      alertType = AlertType.SystemAlert,
      source = source,
      message = message,
      timestamp = timestamp
    )