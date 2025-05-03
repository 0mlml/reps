package com.reps

import com.reps.models._
import com.reps.services._
import com.reps.utils.DateUtils

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.io.StdIn
import scala.util.{Try, Success, Failure}

object Main:
  val fileService = new FileServiceImpl()
  val dataCollectionService = new DataCollectionServiceImpl(fileService)
  val dataAnalysisService = new DataAnalysisServiceImpl(fileService)
  val alertService = new AlertServiceImpl()
  val visualizationService = new VisualizationServiceImpl()
  val specificDateTime = LocalDateTime.of(2025, 3, 31, 18, 0)

  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  def main(args: Array[String]): Unit =
    println("Renewable Energy Plant System (REPS)")
    println("====================================")

    mainMenuLoop()

  def mainMenuLoop(): Unit =
    displayMainMenu()
    val choice = StdIn.readLine("Enter your choice: ")

    choice match
      case "1" =>
        monitorEnergyData()
        mainMenuLoop()
      case "2" =>
        collectAndStoreData()
        mainMenuLoop()
      case "3" =>
        visualizeEnergyData()
        mainMenuLoop()
      case "4" =>
        analyzeEnergyData()
        mainMenuLoop()
      case "5" =>
        checkAlerts()
        mainMenuLoop()
      case "0" =>
        println("Exiting REPS. Goodbye!")
      case _ =>
        println("Invalid choice. Please try again.")
        mainMenuLoop()

  def displayMainMenu(): Unit =
    println("\nMain Menu:")
    println("1. Monitor Energy Data")
    println("2. Collect and Store Data")
    println("3. Visualize Energy Data")
    println("4. Analyze Energy Data")
    println("5. Check Alerts")
    println("0. Exit")

  def selectEnergyType(): EnergyType =
    println("\nSelect Energy Type:")
    println("1. Solar")
    println("2. Wind")
    println("3. Hydro")
    println("4. Total Energy Production")

    val choice = StdIn.readLine("Enter your choice: ")
    choice match
      case "1" => EnergyType.Solar
      case "2" => EnergyType.Wind
      case "3" => EnergyType.Hydro
      case "4" => EnergyType.Total
      case _ =>
        println("Invalid choice.")
        selectEnergyType()

  def selectTimeRange(): (LocalDateTime, LocalDateTime) =
    println("\nSelect Time Range:")
    println("1. Last Hour")
    println("2. Today")
    println("3. This Week")
    println("4. This Month")
    println("5. Custom Range")

    val choice = StdIn.readLine("Enter your choice: ")
    choice match
      case "1" =>
        val now = specificDateTime
        (now.minusHours(1), now)

      case "2" =>
        (DateUtils.startOfDay(), DateUtils.endOfDay())

      case "3" =>
        (DateUtils.startOfWeek(), DateUtils.endOfWeek())

      case "4" =>
        (DateUtils.startOfMonth(), DateUtils.endOfMonth())

      case "5" =>
        inputCustomTimeRange()

      case _ =>
        println("Invalid choice.")
        selectTimeRange()

  def selectTimeRangeNoHour(): (LocalDateTime, LocalDateTime) =
    println("\nSelect Time Range:")
    println("1. Today")
    println("2. This Week")
    println("3. This Month")
    println("4. Custom Range")

    val choice = StdIn.readLine("Enter your choice: ")
    choice match
      case "1" =>
        (DateUtils.startOfDay(), DateUtils.endOfDay())

      case "2" =>
        (DateUtils.startOfWeek(), DateUtils.endOfWeek())

      case "3" =>
        (DateUtils.startOfMonth(), DateUtils.endOfMonth())

      case "4" =>
        inputCustomTimeRange()

      case _ =>
        println("Invalid choice.")
        selectTimeRangeNoHour()

  def inputCustomTimeRange(): (LocalDateTime, LocalDateTime) =
    println("\nEnter custom time range.")

    def getValidStartTime(): LocalDateTime =
      val startStr = StdIn.readLine("Start time (yyyy-MM-dd HH:mm): ")
      Try(LocalDateTime.parse(startStr, dateFormatter)) match
        case Success(time) => time
        case Failure(_) =>
          println("Invalid format. Please use yyyy-MM-dd HH:mm")
          getValidStartTime()

    def getValidEndTime(startTime: LocalDateTime): LocalDateTime =
      val endStr = StdIn.readLine("End time (yyyy-MM-dd HH:mm): ")
      Try(LocalDateTime.parse(endStr, dateFormatter)) match
        case Success(time) =>
          if time.isAfter(startTime) then time
          else
            println("End time must be after start time.")
            getValidEndTime(startTime)
        case Failure(_) =>
          println("Invalid format. Please use yyyy-MM-dd HH:mm")
          getValidEndTime(startTime)

    val startTime = getValidStartTime()
    val endTime = getValidEndTime(startTime)
    (startTime, endTime)

  def selectTimeGrouping(): TimeGrouping =
    println("\nSelect Time Grouping:")
    println("1. Hourly")
    println("2. Daily")
    println("3. Weekly")
    println("4. Monthly")

    val choice = StdIn.readLine("Enter your choice: ")
    choice match
      case "1" => TimeGrouping.Hourly
      case "2" => TimeGrouping.Daily
      case "3" => TimeGrouping.Weekly
      case "4" => TimeGrouping.Monthly
      case _ =>
        println("Invalid choice.")
        selectTimeGrouping()

  def monitorEnergyData(): Unit =
    println("\n--- Energy Monitoring ---")

    val energyType = selectEnergyType()
    val (startTime, endTime) = selectTimeRange()

    println(s"\nMonitoring ${energyType} from ${startTime
        .format(dateFormatter)} to ${endTime.format(dateFormatter)}...")

    fileService.readEnergyData(energyType, startTime, endTime) match
      case Success(data) =>
        if data.isEmpty then
          println("No data available for the selected period.")
        else
          val report = visualizationService.generateEnergyReport(
            data,
            energyType,
            startTime,
            endTime
          )
          println(report)

      case Failure(exception) =>
        println(s"Error retrieving data: ${exception.getMessage}")

  def collectAndStoreData(): Unit =
    println("\n--- Data Collection ---")

    val energyType = selectEnergyType()
    val (startTime, endTime) = selectTimeRange()

    println("Enter energy value (MW):")
    val valueStr = StdIn.readLine()

    Try(valueStr.toDouble) match
      case Success(value) =>
        if (value < 0) {
          println("Error: Recorded energy value cannot be negative.")
        } else {
          val data = EnergyData(energyType, startTime, endTime, value)
          dataCollectionService.storeData(List(data)) match
            case Success(_) =>
              println("Data stored successfully.")
            case Failure(exception) =>
              println(s"Error storing data: ${exception.getMessage}")
        }

      case Failure(_) =>
        println("Invalid value. Please enter a valid number.")

  def visualizeEnergyData(): Unit =
    println("\n--- Energy Visualization ---")

    val energyType = selectEnergyType()
    val (startTime, endTime) = selectTimeRange()

    println(s"\nVisualizing ${energyType} from ${startTime
        .format(dateFormatter)} to ${endTime.format(dateFormatter)}...")

    fileService.readEnergyData(energyType, startTime, endTime) match
      case Success(data) =>
        if data.isEmpty then
          println("No data available for the selected period.")
        else
          val report = visualizationService.generateEnergyReport(
            data,
            energyType,
            startTime,
            endTime
          )
          println(report)

      case Failure(exception) =>
        println(s"Error retrieving data: ${exception.getMessage}")

  def analyzeEnergyData(): Unit =
    println("\n--- Data Analysis ---")

    val energyType = selectEnergyType()
    val (startTime, endTime) = selectTimeRange()
    val grouping = selectTimeGrouping()

    println(
      s"\nAnalyzing ${energyType} from ${startTime.format(dateFormatter)} to ${endTime
          .format(dateFormatter)} with ${grouping} grouping..."
    )

    dataAnalysisService.analyzeData(
      energyType,
      startTime,
      endTime,
      grouping
    ) match
      case Success(stats) =>
        val report = visualizationService.generateStatisticsReport(stats)
        println(report)

      case Failure(exception) =>
        println(s"Error analyzing data: ${exception.getMessage}")

  def checkAlerts(): Unit =
    println("\n--- Alert Checking ---")

    println("Checking for alerts across all energy types...")
    println(
      "This will analyze data for all energy sources to detect potential issues."
    )

    val (startTime, endTime) = selectTimeRangeNoHour()
    println(
      s"\nChecking alerts from ${startTime.format(dateFormatter)} to ${endTime.format(dateFormatter)}..."
    )

    val solarDataTry =
      fileService.readEnergyData(EnergyType.Solar, startTime, endTime)
    val windDataTry =
      fileService.readEnergyData(EnergyType.Wind, startTime, endTime)
    val hydroDataTry =
      fileService.readEnergyData(EnergyType.Hydro, startTime, endTime)
    val totalDataTry =
      fileService.readEnergyData(EnergyType.Total, startTime, endTime)

    val historicalStartTime = startTime.minusWeeks(4)
    val solarHistoryTry =
      fileService.readEnergyData(
        EnergyType.Solar,
        historicalStartTime,
        startTime
      )
    val windHistoryTry =
      fileService.readEnergyData(
        EnergyType.Wind,
        historicalStartTime,
        startTime
      )
    val hydroHistoryTry =
      fileService.readEnergyData(
        EnergyType.Hydro,
        historicalStartTime,
        startTime
      )

    if solarDataTry.isSuccess && windDataTry.isSuccess && hydroDataTry.isSuccess &&
      totalDataTry.isSuccess && solarHistoryTry.isSuccess && windHistoryTry.isSuccess &&
      hydroHistoryTry.isSuccess
    then

      val solarData = solarDataTry.get
      val windData = windDataTry.get
      val hydroData = hydroDataTry.get
      val totalData = totalDataTry.get

      val historicalData = Map(
        EnergyType.Solar -> solarHistoryTry.get,
        EnergyType.Wind -> windHistoryTry.get,
        EnergyType.Hydro -> hydroHistoryTry.get
      )

      if solarData.isEmpty && windData.isEmpty && hydroData.isEmpty && totalData.isEmpty
      then println("No data available for the selected period.")
      else
        val alerts = alertService.generateAlerts(
          solarData,
          windData,
          hydroData,
          totalData,
          historicalData
        )

        if alerts.isEmpty then
          println("No alerts detected for the selected period.")
        else
          val alertsByDay =
            alerts.groupBy(_.timestamp.toLocalDate).toList.sortBy(_._1)

          println(s"\nFound ${alerts.size} alerts:")
          println("-" * 80)

          def printAlertGroups(
              groups: List[(java.time.LocalDate, List[Alert])]
          ): Unit =
            groups match
              case Nil => ()
              case (day, dayAlerts) :: tail =>
                println(s"\nDate: $day")
                println("-" * 40)

                val sortedAlerts = dayAlerts.sortBy(_.source.toString)
                printAlerts(sortedAlerts)

                printAlertGroups(tail)

          def printAlerts(alerts: List[Alert]): Unit =
            alerts match
              case Nil => ()
              case alert :: tail =>
                println(s"[${alert.source}] ${alert.message}")
                printAlerts(tail)

          printAlertGroups(alertsByDay)
    else
      val errors = List(
        solarDataTry.failed.toOption.map(e => s"Solar data: ${e.getMessage}"),
        windDataTry.failed.toOption.map(e => s"Wind data: ${e.getMessage}"),
        hydroDataTry.failed.toOption.map(e => s"Hydro data: ${e.getMessage}"),
        totalDataTry.failed.toOption.map(e => s"Total data: ${e.getMessage}"),
        solarHistoryTry.failed.toOption.map(e =>
          s"Solar history: ${e.getMessage}"
        ),
        windHistoryTry.failed.toOption.map(e =>
          s"Wind history: ${e.getMessage}"
        ),
        hydroHistoryTry.failed.toOption.map(e =>
          s"Hydro history: ${e.getMessage}"
        )
      ).flatMap(x => x) 

      println("Error retrieving data:")

      def printErrors(errors: List[String]): Unit =
        errors match
          case Nil => ()
          case err :: tail =>
            println(s"- $err")
            printErrors(tail)

      printErrors(errors)
