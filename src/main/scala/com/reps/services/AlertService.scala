package com.reps.services
import scala.collection.immutable._
import com.reps.models._
import scala.util.Try
import java.time.{LocalDateTime, LocalDate}
import java.util.{Map => JMap, HashMap => JHashMap}

trait AlertService:
  def generateAlerts(
    solarData: List[EnergyData], 
    windData: List[EnergyData], 
    hydroData: List[EnergyData],
    totalData: List[EnergyData],
    historicalData: Map[EnergyType, List[EnergyData]]
  ): List[Alert]

class AlertServiceImpl:
  def generateAlerts(
    solarData: List[EnergyData], 
    windData: List[EnergyData], 
    hydroData: List[EnergyData],
    totalData: List[EnergyData],
    historicalData: Map[EnergyType, List[EnergyData]]
  ): List[Alert] =
    val allAlerts = 
      checkLowGeneration(solarData, windData, hydroData, historicalData) ++
      checkZeroGeneration(windData, hydroData) ++
      checkDuplicateReadings(solarData, windData, hydroData) ++
      checkRenewablePercentage(solarData, windData, hydroData, totalData)
    
    groupAlertsByDayAndType(allAlerts)
  
  private def groupAlertsByDayAndType(alerts: List[Alert]): List[Alert] =
    val groupedAlerts = alerts.groupBy(alert => (
      alert.timestamp.toLocalDate, 
      alert.source
    ))
    
    groupedAlerts.map { case (_, alertsForDayAndType) =>
      alertsForDayAndType.sortBy { alert =>
        alert.message match
          case msg if msg.contains("renewable percentage") => 0
          case msg if msg.contains("zero generation") => 1
          case msg if msg.contains("repeated readings") => 2
          case _ => 3
      }.head
    }.toList
  
  private def checkLowGeneration(
    solarData: List[EnergyData], 
    windData: List[EnergyData], 
    hydroData: List[EnergyData],
    historicalData: Map[EnergyType, List[EnergyData]]
  ): List[Alert] =
    val solarByDay = groupByDay(solarData)
    val windByDay = groupByDay(windData)
    val hydroByDay = groupByDay(hydroData)
    
    val solarWeeklyAvg = calculateWeeklyAverage(historicalData.getOrElse(EnergyType.Solar, List.empty))
    val windWeeklyAvg = calculateWeeklyAverage(historicalData.getOrElse(EnergyType.Wind, List.empty))
    val hydroWeeklyAvg = calculateWeeklyAverage(historicalData.getOrElse(EnergyType.Hydro, List.empty))
    
    val solarAlerts = checkSolarLowGeneration(solarByDay.toList, solarWeeklyAvg, List.empty)
    val windAlerts = checkWindLowGeneration(windByDay.toList, windWeeklyAvg, List.empty)
    val hydroAlerts = checkHydroLowGeneration(hydroByDay.toList, hydroWeeklyAvg, List.empty)
    
    solarAlerts ++ windAlerts ++ hydroAlerts
  
  private def checkSolarLowGeneration(
    solarByDayEntries: List[(LocalDate, List[EnergyData])], 
    solarWeeklyAvg: Double,
    alerts: List[Alert]
  ): List[Alert] =
    solarByDayEntries match
      case Nil => alerts
      case (day, dayData) :: rest =>
        val daytimeSolarData = dayData.filter(data => 
          data.startTime.getHour >= 6 && data.startTime.getHour < 19
        )
        
        val newAlerts = if daytimeSolarData.nonEmpty then
          val dailyAvg = daytimeSolarData.map(_.value).sum / daytimeSolarData.size
          if solarWeeklyAvg > 0 && dailyAvg < solarWeeklyAvg * 0.8 then
            val lowestReading = daytimeSolarData.minBy(_.value)
            Alert(
              AlertType.LowOutput,
              EnergyType.Solar,
              s"Low solar generation detected: ${dailyAvg.toInt} MW daily average is 20% below weekly average of ${solarWeeklyAvg.toInt} MW. Lowest reading: ${lowestReading.value.toInt} MW at ${lowestReading.startTime}",
              day.atStartOfDay()
            ) :: alerts
          else
            alerts
        else
          alerts
        
        checkSolarLowGeneration(rest, solarWeeklyAvg, newAlerts)
  
  private def checkWindLowGeneration(
    windByDayEntries: List[(LocalDate, List[EnergyData])], 
    windWeeklyAvg: Double,
    alerts: List[Alert]
  ): List[Alert] =
    windByDayEntries match
      case Nil => alerts
      case (day, dayData) :: rest =>
        val dailyAvg = dayData.map(_.value).sum / dayData.size
        val newAlerts = if windWeeklyAvg > 0 && dailyAvg < windWeeklyAvg * 0.8 then
          val lowestReading = dayData.minBy(_.value)
          Alert(
            AlertType.LowOutput,
            EnergyType.Wind,
            s"Low wind generation detected: ${dailyAvg.toInt} MW daily average is 20% below weekly average of ${windWeeklyAvg.toInt} MW. Lowest reading: ${lowestReading.value.toInt} MW at ${lowestReading.startTime}",
            day.atStartOfDay()
          ) :: alerts
        else
          alerts
        
        checkWindLowGeneration(rest, windWeeklyAvg, newAlerts)
  
  private def checkHydroLowGeneration(
    hydroByDayEntries: List[(LocalDate, List[EnergyData])], 
    hydroWeeklyAvg: Double,
    alerts: List[Alert]
  ): List[Alert] =
    hydroByDayEntries match
      case Nil => alerts
      case (day, dayData) :: rest =>
        val dailyAvg = dayData.map(_.value).sum / dayData.size
        val newAlerts = if hydroWeeklyAvg > 0 && dailyAvg < hydroWeeklyAvg * 0.8 then
          val lowestReading = dayData.minBy(_.value)
          Alert(
            AlertType.LowOutput,
            EnergyType.Hydro,
            s"Low hydro generation detected: ${dailyAvg.toInt} MW daily average is 20% below weekly average of ${hydroWeeklyAvg.toInt} MW. Lowest reading: ${lowestReading.value.toInt} MW at ${lowestReading.startTime}",
            day.atStartOfDay()
          ) :: alerts
        else
          alerts
        
        checkHydroLowGeneration(rest, hydroWeeklyAvg, newAlerts)
  
  private def checkZeroGeneration(
    windData: List[EnergyData], 
    hydroData: List[EnergyData]
  ): List[Alert] =
    // Group by day
    val windByDay = groupByDay(windData)
    val hydroByDay = groupByDay(hydroData)
    
    // Check wind data for zero readings
    val windAlerts = processWindZeroReadings(windByDay.toList, List.empty)
    val hydroAlerts = processHydroZeroReadings(hydroByDay.toList, List.empty)
    
    windAlerts ++ hydroAlerts
  
  private def processWindZeroReadings(
    windByDayEntries: List[(LocalDate, List[EnergyData])],
    alerts: List[Alert]
  ): List[Alert] =
    windByDayEntries match
      case Nil => alerts
      case (day, dayData) :: rest =>
        val zeroReadings = dayData.filter(_.value == 0.0)
        val newAlerts = if zeroReadings.nonEmpty then
          // Find the reading with the longest duration at zero
          val significantZeroReading = zeroReadings.maxBy(data => 
            java.time.Duration.between(data.startTime, data.endTime).toMinutes
          )
          
          Alert(
            AlertType.EquipmentMalfunction,
            EnergyType.Wind,
            s"Zero wind generation detected: ${zeroReadings.size} zero readings on ${day}, possibly faulty equipment. Most significant: ${significantZeroReading.startTime} to ${significantZeroReading.endTime}",
            day.atStartOfDay()
          ) :: alerts
        else
          alerts
        
        processWindZeroReadings(rest, newAlerts)
  
  private def processHydroZeroReadings(
    hydroByDayEntries: List[(LocalDate, List[EnergyData])],
    alerts: List[Alert]
  ): List[Alert] =
    hydroByDayEntries match
      case Nil => alerts
      case (day, dayData) :: rest =>
        val zeroReadings = dayData.filter(_.value == 0.0)
        val newAlerts = if zeroReadings.nonEmpty then
          // Find the reading with the longest duration at zero
          val significantZeroReading = zeroReadings.maxBy(data => 
            java.time.Duration.between(data.startTime, data.endTime).toMinutes
          )
          
          Alert(
            AlertType.EquipmentMalfunction,
            EnergyType.Hydro,
            s"Zero hydro generation detected: ${zeroReadings.size} zero readings on ${day}, possibly faulty equipment. Most significant: ${significantZeroReading.startTime} to ${significantZeroReading.endTime}",
            day.atStartOfDay()
          ) :: alerts
        else
          alerts
        
        processHydroZeroReadings(rest, newAlerts)
  
  private def checkDuplicateReadings(
    solarData: List[EnergyData], 
    windData: List[EnergyData], 
    hydroData: List[EnergyData]
  ): List[Alert] =
    // Group by day
    val solarByDay = groupByDay(solarData)
    val windByDay = groupByDay(windData)
    val hydroByDay = groupByDay(hydroData)
    
    // Check for duplicate readings
    val solarAlerts = processDuplicateSolarReadings(solarByDay.toList, List.empty)
    val windAlerts = processDuplicateWindReadings(windByDay.toList, List.empty)
    val hydroAlerts = processDuplicateHydroReadings(hydroByDay.toList, List.empty)
    
    solarAlerts ++ windAlerts ++ hydroAlerts
  
  private def processDuplicateSolarReadings(
    solarByDayEntries: List[(LocalDate, List[EnergyData])],
    alerts: List[Alert]
  ): List[Alert] =
    solarByDayEntries match
      case Nil => alerts
      case (day, dayData) :: rest =>
        val sortedData = dayData.sortBy(_.startTime).filter(_.value > 0)
        
        val duplicates = findConsecutiveDuplicates(sortedData)
        val newAlerts = if duplicates.nonEmpty then
          val firstOccurrence = duplicates.head
          Alert(
            AlertType.SystemError,
            EnergyType.Solar,
            s"Reading error: Same value (${firstOccurrence.value} MW) repeated for three consecutive readings starting at ${firstOccurrence.startTime}",
            day.atStartOfDay()
          ) :: alerts
        else
          alerts
        
        processDuplicateSolarReadings(rest, newAlerts)
  
  private def processDuplicateWindReadings(
    windByDayEntries: List[(LocalDate, List[EnergyData])],
    alerts: List[Alert]
  ): List[Alert] =
    windByDayEntries match
      case Nil => alerts
      case (day, dayData) :: rest =>
        val sortedData = dayData.sortBy(_.startTime)
        
        val duplicates = findConsecutiveDuplicates(sortedData)
        val newAlerts = if duplicates.nonEmpty then
          val firstOccurrence = duplicates.head
          Alert(
            AlertType.SystemError,
            EnergyType.Wind,
            s"Reading error: Same value (${firstOccurrence.value} MW) repeated for three consecutive readings starting at ${firstOccurrence.startTime}",
            day.atStartOfDay()
          ) :: alerts
        else
          alerts
        
        processDuplicateWindReadings(rest, newAlerts)
  
  private def processDuplicateHydroReadings(
    hydroByDayEntries: List[(LocalDate, List[EnergyData])],
    alerts: List[Alert]
  ): List[Alert] =
    hydroByDayEntries match
      case Nil => alerts
      case (day, dayData) :: rest =>
        val sortedData = dayData.sortBy(_.startTime)
        
        val duplicates = findConsecutiveDuplicates(sortedData)
        val newAlerts = if duplicates.nonEmpty then
          val firstOccurrence = duplicates.head
          Alert(
            AlertType.SystemError,
            EnergyType.Hydro,
            s"Reading error: Same value (${firstOccurrence.value} MW) repeated for three consecutive readings starting at ${firstOccurrence.startTime}",
            day.atStartOfDay()
          ) :: alerts
        else
          alerts
        
        processDuplicateHydroReadings(rest, newAlerts)
  
  private def checkRenewablePercentage(
    solarData: List[EnergyData], 
    windData: List[EnergyData], 
    hydroData: List[EnergyData],
    totalData: List[EnergyData]
  ): List[Alert] =
    // Group by day
    val solarByDay = groupByDay(solarData)
    val windByDay = groupByDay(windData)
    val hydroByDay = groupByDay(hydroData)
    val totalByDay = groupByDay(totalData)
    
    // Calculate daily totals for each energy type and total grid
    val days = (solarByDay.keys ++ windByDay.keys ++ hydroByDay.keys ++ totalByDay.keys).toSet
    
    processRenewablePercentage(days.toList, solarByDay, windByDay, hydroByDay, totalByDay, List.empty)
  
  private def processRenewablePercentage(
    days: List[LocalDate],
    solarByDay: Map[LocalDate, List[EnergyData]],
    windByDay: Map[LocalDate, List[EnergyData]],
    hydroByDay: Map[LocalDate, List[EnergyData]],
    totalByDay: Map[LocalDate, List[EnergyData]],
    alerts: List[Alert]
  ): List[Alert] =
    days match
      case Nil => alerts
      case day :: rest =>
        val solarTotal = solarByDay.get(day).map(data => data.map(_.value).sum).getOrElse(0.0)
        val windTotal = windByDay.get(day).map(data => data.map(_.value).sum).getOrElse(0.0)
        val hydroTotal = hydroByDay.get(day).map(data => data.map(_.value).sum).getOrElse(0.0)
        val gridTotal = totalByDay.get(day).map(data => data.map(_.value).sum).getOrElse(0.0)
        
        val newAlerts = if gridTotal > 0 then
          val renewableTotal = solarTotal + windTotal + hydroTotal
          val renewablePercentage = (renewableTotal / gridTotal) * 100
          
          if renewablePercentage < 40.0 then
            // Find the hour with the lowest renewable percentage
            val hourlyData = getHourlyData(day, solarByDay, windByDay, hydroByDay, totalByDay)
            
            // Find the hour with the lowest renewable percentage
            val worstHour = hourlyData.minBy { case (hour, (renewableValue, totalValue)) =>
              if totalValue > 0 then renewableValue / totalValue else Double.MaxValue
            }
            
            val (hour, (renewableValue, totalValue)) = worstHour
            val hourlyPercentage = if totalValue > 0 then (renewableValue / totalValue) * 100 else 0.0
            
            Alert(
              AlertType.LowOutput,
              EnergyType.Total,
              s"Low renewable generation: Only ${renewablePercentage.toInt}% of total grid power from renewable sources on ${day}. Worst at ${hour}:00 with ${hourlyPercentage.toInt}% (${renewableValue.toInt} MW of ${totalValue.toInt} MW)",
              day.atStartOfDay()
            ) :: alerts
          else
            alerts
        else
          alerts
        
        processRenewablePercentage(rest, solarByDay, windByDay, hydroByDay, totalByDay, newAlerts)
  
  // Helper function to group energy data by day
  private def groupByDay(data: List[EnergyData]): Map[LocalDate, List[EnergyData]] =
    data.groupBy(_.startTime.toLocalDate)
  
  // Helper function to calculate weekly average from historical data
  private def calculateWeeklyAverage(data: List[EnergyData]): Double =
    if data.isEmpty then 0.0
    else data.map(_.value).sum / data.size
  
  // Helper function to find consecutive duplicates (three or more in a row)
  private def findConsecutiveDuplicates(sortedData: List[EnergyData]): List[EnergyData] =
    if sortedData.size < 3 then return List.empty
    
    findConsecutiveDuplicatesRec(sortedData, 0, List.empty)
  
  private def findConsecutiveDuplicatesRec(
    sortedData: List[EnergyData], 
    index: Int, 
    duplicates: List[EnergyData]
  ): List[EnergyData] =
    if index >= sortedData.size - 2 then
      duplicates
    else
      val current = sortedData(index)
      val next1 = sortedData(index + 1)
      val next2 = sortedData(index + 2)
      
      if current.value == next1.value && current.value == next2.value then
        findConsecutiveDuplicatesRec(sortedData, index + 1, current :: duplicates)
      else
        findConsecutiveDuplicatesRec(sortedData, index + 1, duplicates)
  
  // Helper function to get hourly data for renewable percentage calculation
  private def getHourlyData(
    day: LocalDate,
    solarByDay: Map[LocalDate, List[EnergyData]],
    windByDay: Map[LocalDate, List[EnergyData]],
    hydroByDay: Map[LocalDate, List[EnergyData]],
    totalByDay: Map[LocalDate, List[EnergyData]]
  ): Map[Int, (Double, Double)] =
    val initialHourlyMap = (0 to 23).map(hour => (hour, (0.0, 0.0))).toMap
    
    val withSolar = processSolarHourlyData(solarByDay.getOrElse(day, List.empty), day, initialHourlyMap)
    val withWind = processWindHourlyData(windByDay.getOrElse(day, List.empty), day, withSolar)
    val withHydro = processHydroHourlyData(hydroByDay.getOrElse(day, List.empty), day, withWind)
    val withTotal = processTotalHourlyData(totalByDay.getOrElse(day, List.empty), day, withHydro)
    
    withTotal
  
  private def processSolarHourlyData(
    solarData: List[EnergyData],
    day: LocalDate,
    hourlyMap: Map[Int, (Double, Double)]
  ): Map[Int, (Double, Double)] =
    solarData match
      case Nil => hourlyMap
      case data :: rest =>
        val hour = data.startTime.getHour
        val (currentRenewable, currentTotal) = hourlyMap(hour)
        val updatedMap = hourlyMap + (hour -> (currentRenewable + data.value, currentTotal))
        processSolarHourlyData(rest, day, updatedMap)
  
  private def processWindHourlyData(
    windData: List[EnergyData],
    day: LocalDate,
    hourlyMap: Map[Int, (Double, Double)]
  ): Map[Int, (Double, Double)] =
    windData match
      case Nil => hourlyMap
      case data :: rest =>
        val hour = data.startTime.getHour
        val (currentRenewable, currentTotal) = hourlyMap(hour)
        val updatedMap = hourlyMap + (hour -> (currentRenewable + data.value, currentTotal))
        processWindHourlyData(rest, day, updatedMap)
  
  private def processHydroHourlyData(
    hydroData: List[EnergyData],
    day: LocalDate,
    hourlyMap: Map[Int, (Double, Double)]
  ): Map[Int, (Double, Double)] =
    hydroData match
      case Nil => hourlyMap
      case data :: rest =>
        val hour = data.startTime.getHour
        val (currentRenewable, currentTotal) = hourlyMap(hour)
        val updatedMap = hourlyMap + (hour -> (currentRenewable + data.value, currentTotal))
        processHydroHourlyData(rest, day, updatedMap)
  
  private def processTotalHourlyData(
    totalData: List[EnergyData],
    day: LocalDate,
    hourlyMap: Map[Int, (Double, Double)]
  ): Map[Int, (Double, Double)] =
    totalData match
      case Nil => hourlyMap
      case data :: rest =>
        val hour = data.startTime.getHour
        val (currentRenewable, _) = hourlyMap(hour)
        val updatedMap = hourlyMap + (hour -> (currentRenewable, data.value))
        processTotalHourlyData(rest, day, updatedMap)