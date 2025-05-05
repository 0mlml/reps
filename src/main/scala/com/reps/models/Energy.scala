package com.reps.models
import scala.collection.immutable._
enum EnergyType:
  case Solar, Wind, Hydro, Total

case class Energy(
  energyType: EnergyType,
  description: String
)

object Energy:
  val Solar = Energy(EnergyType.Solar, "Aurinkovoiman tuotantoennuste - päivitys kerran vuorokaudessa")
  val Wind = Energy(EnergyType.Wind, "Tuulivoimatuotanto - reaaliaikatieto")
  val Hydro = Energy(EnergyType.Hydro, "Hydro power production - real time data")
  val Total = Energy(EnergyType.Total, "Electricity production in Finland")