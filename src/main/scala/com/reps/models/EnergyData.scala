package com.reps.models
import scala.collection.immutable._
import java.time.LocalDateTime

case class EnergyData(
  energyType: EnergyType,
  startTime: LocalDateTime,
  endTime: LocalDateTime,
  value: Double
)

object EnergyData:
  def apply(energyType: EnergyType, startTime: LocalDateTime, endTime: LocalDateTime, value: Double): EnergyData =
    new EnergyData(energyType, startTime, endTime, value)