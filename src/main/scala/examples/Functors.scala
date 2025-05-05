package com.reps.examples

import com.reps.models._
import java.time.LocalDateTime

/** A generic Functor trait that defines the mapping operation for any type
  * constructor F[_]
  */
trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def identityLaw[A](fa: F[A]): Boolean =
    map(fa)(identity) == fa

  def compositionLaw[A, B, C](fa: F[A], f: B => C, g: A => B): Boolean =
    map(fa)(f compose g) == map(map(fa)(g))(f)

/** Implementation of Functor for EnergyData
  *
  * Since EnergyData is not a type constructor but a concrete type, we use a
  * type alias to create a type constructor that wraps Double (the value in
  * EnergyData)
  */
object EnergyFunctor:
  type EnergyDataT[A] = EnergyData

  given Functor[EnergyDataT] with
    def map[A, B](fa: EnergyDataT[A])(f: A => B): EnergyDataT[B] =
      val data = fa.asInstanceOf[EnergyData]
      EnergyData(
        energyType = data.energyType,
        startTime = data.startTime,
        endTime = data.endTime,
        value = f(data.value.asInstanceOf[A]).asInstanceOf[Double]
      )

/** Usage examples showing how to apply the functor concept to the REPS codebase
  */
object FunctorExamples:
  import EnergyFunctor.given

  def convertToKilowatts(data: EnergyData): EnergyData =
    summon[Functor[EnergyFunctor.EnergyDataT]]
      .map[Double, Double](data)(_ * 1000)

  def transformEnergyData[F[_]](
      data: List[F[Double]],
      functor: Functor[F],
      transformation: Double => Double
  ): List[F[Double]] =
    data.map(entry => functor.map(entry)(transformation))

  def scaleAllReadings(
      readings: List[EnergyData],
      scaleFactor: Double
  ): List[EnergyData] =
    transformEnergyData[EnergyFunctor.EnergyDataT](
      readings.asInstanceOf[List[EnergyFunctor.EnergyDataT[Double]]],
      summon[Functor[EnergyFunctor.EnergyDataT]],
      _ * scaleFactor
    ).asInstanceOf[List[EnergyData]]

  def normalizeAndConvertTokW(
      readings: List[EnergyData],
      maxCapacity: Double
  ): List[EnergyData] =
    val normalizeTransform = (value: Double) => value / maxCapacity
    val kWTransform = (value: Double) => value * 1000

    val composedTransform = kWTransform compose normalizeTransform

    transformEnergyData[EnergyFunctor.EnergyDataT](
      readings.asInstanceOf[List[EnergyFunctor.EnergyDataT[Double]]],
      summon[Functor[EnergyFunctor.EnergyDataT]],
      composedTransform
    ).asInstanceOf[List[EnergyData]]

  def main(args: Array[String]): Unit =
    val now = LocalDateTime.now()

    val solarReading =
      EnergyData(EnergyType.Solar, now, now.plusHours(1), 150.0)
    val windReading = EnergyData(EnergyType.Wind, now, now.plusHours(1), 220.0)
    val hydroReading =
      EnergyData(EnergyType.Hydro, now, now.plusHours(1), 350.0)

    val readings = List(solarReading, windReading, hydroReading)

    val kWReadings = scaleAllReadings(readings, 1000)

    println("Original readings (MW):")
    readings.foreach(data => println(s"${data.energyType}: ${data.value} MW"))

    println("\nConverted readings (kW):")
    kWReadings.foreach(data => println(s"${data.energyType}: ${data.value} kW"))

    val normalizedReadings = normalizeAndConvertTokW(readings, 500.0)

    println("\nNormalized and converted readings:")
    normalizedReadings.foreach(data =>
      println(
        s"${data.energyType}: ${data.value} kW (${data.value / 1000 * 100}%)"
      )
    )
