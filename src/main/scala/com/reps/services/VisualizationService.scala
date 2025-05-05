package com.reps.services
import scala.collection.immutable._
import com.reps.models._
import java.time.{LocalDateTime, format}
import java.time.format.DateTimeFormatter
import scala.util.Try

trait VisualizationService:
  def generateEnergyReport(
    data: List[EnergyData],
    energyType: EnergyType,
    startTime: LocalDateTime,
    endTime: LocalDateTime
  ): String
  
  def generateAlertReport(alerts: List[Alert]): String
  
  def generateStatisticsReport(statistics: Statistics): String

class VisualizationServiceImpl extends VisualizationService:
  private val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  
  def generateEnergyReport(
    data: List[EnergyData],
    energyType: EnergyType,
    startTime: LocalDateTime,
    endTime: LocalDateTime
  ): String =
    if data.isEmpty then return "No data available for the selected period."
    
    val header = s"Energy Report - ${energyType} Production\n"
    val timeRange = s"Period: ${startTime.format(dateFormatter)} to ${endTime.format(dateFormatter)}\n"
    val separator = "-" * 80 + "\n"
    
    val tableHeader = f"| ${"Start Time"}%-20s | ${"End Time"}%-20s | ${"Value (MW)"}%-15s |\n"
    val tableSeparator = s"| ${"-" * 20} | ${"-" * 20} | ${"-" * 15} |\n"
    
    val tableRows = data.sortBy(_.startTime).map { entry =>
      f"| ${entry.startTime.format(dateFormatter)}%-20s | ${entry.endTime.format(dateFormatter)}%-20s | ${entry.value}%-15.2f |\n"
    }.mkString
    
    val totalProduction = data.map(_.value).sum
    val averageProduction = if data.nonEmpty then data.map(_.value).sum / data.size else 0
    
    val summary = s"\nTotal Production: $totalProduction MW\n"
    val average = f"Average Production: $averageProduction%.2f MW\n"
    
    header + timeRange + separator + tableHeader + tableSeparator + tableRows + separator + summary + average
  
  def generateAlertReport(alerts: List[Alert]): String =
    if alerts.isEmpty then return "No alerts for the selected period."
    
    val header = "Alert Report\n"
    val separator = "-" * 80 + "\n"
    
    val tableHeader = f"| ${"Timestamp"}%-20s | ${"Type"}%-20s | ${"Source"}%-10s | ${"Message"}%-30s |\n"
    val tableSeparator = s"| ${"-" * 20} | ${"-" * 10} | ${"-" * 20} | ${"-" * 10} | ${"-" * 30} |\n"
    
    val tableRows = alerts.sortBy(_.timestamp).map { alert =>
      f"| ${alert.timestamp.format(dateFormatter)}%-20s | ${alert.alertType}%-20s | ${alert.source}%-10s | ${alert.message}%-30s |\n"
    }.mkString
    
    header + separator + tableHeader + tableSeparator + tableRows + separator 
  
  def generateStatisticsReport(statistics: Statistics): String =
    val header = s"Statistical Analysis - ${statistics.energyType} Production\n"
    val timeRange = s"Period: ${statistics.timeRange.startTime.format(dateFormatter)} to ${statistics.timeRange.endTime.format(dateFormatter)}\n"
    val grouping = s"Grouping: ${statistics.grouping}\n"
    val separator = "-" * 80 + "\n"
    
    val mean = f"Mean: ${statistics.mean}%.2f MW\n"
    val median = f"Median: ${statistics.median}%.2f MW\n"
    val mode = statistics.mode.map(v => f"Mode: $v%.2f MW\n").getOrElse("Mode: No mode found\n")
    val range = f"Range: ${statistics.range}%.2f MW\n"
    val midrange = f"Midrange: ${statistics.midrange}%.2f MW\n"
    
    header + timeRange + grouping + separator + mean + median + mode + range + midrange