package com.reps.services
import com.reps.models._
import com.reps.utils.{DateUtils, Statistics as StatsUtil}
import java.time.LocalDateTime
import scala.util.Try
trait DataAnalysisService:
  def analyzeData(
      energyType: EnergyType,
      startTime: LocalDateTime,
      endTime: LocalDateTime,
      grouping: TimeGrouping
  ): Try[Statistics]

  def filterByTimeRange(
      data: List[EnergyData],
      startTime: LocalDateTime,
      endTime: LocalDateTime
  ): List[EnergyData]

  def groupByTime(
      data: List[EnergyData],
      grouping: TimeGrouping
  ): Map[TimeRange, List[EnergyData]]

  def search(
      data: List[EnergyData],
      criteria: EnergyData => Boolean
  ): List[EnergyData]
class DataAnalysisServiceImpl(fileService: FileService)
    extends DataAnalysisService:
  def analyzeData(
      energyType: EnergyType,
      startTime: LocalDateTime,
      endTime: LocalDateTime,
      grouping: TimeGrouping
  ): Try[Statistics] =
    for
      data <- fileService.readEnergyData(energyType, startTime, endTime)
      filteredData = filterByTimeRange(data, startTime, endTime)
      values = filteredData.map(_.value)

      // Calculate statistics
      mean = StatsUtil.calculateMean(values)
      median = StatsUtil.calculateMedian(values)
      mode = StatsUtil.calculateMode(values)
      range = StatsUtil.calculateRange(values)
      midrange = StatsUtil.calculateMidrange(values)
    yield Statistics(
      energyType = energyType,
      timeRange = TimeRange(startTime, endTime),
      grouping = grouping,
      mean = mean,
      median = median,
      mode = mode,
      range = range,
      midrange = midrange
    )

  def filterByTimeRange(
      data: List[EnergyData],
      startTime: LocalDateTime,
      endTime: LocalDateTime
  ): List[EnergyData] =
    data.filter(entry =>
      (entry.startTime.isEqual(startTime) || entry.startTime.isAfter(
        startTime
      )) &&
        (entry.endTime.isEqual(endTime) || entry.endTime.isBefore(endTime))
    )

  def groupByTime(
      data: List[EnergyData],
      grouping: TimeGrouping
  ): Map[TimeRange, List[EnergyData]] =
    grouping match
      case TimeGrouping.Hourly =>
        data.groupBy(entry =>
          TimeRange(
            DateUtils.truncateToHour(entry.startTime),
            DateUtils.truncateToHour(entry.startTime).plusHours(1)
          )
        )

      case TimeGrouping.Daily =>
        data.groupBy(entry =>
          TimeRange(
            DateUtils.truncateToDay(entry.startTime),
            DateUtils.truncateToDay(entry.startTime).plusDays(1)
          )
        )

      case TimeGrouping.Weekly =>
        data.groupBy(entry =>
          TimeRange(
            DateUtils.truncateToWeek(entry.startTime),
            DateUtils.truncateToWeek(entry.startTime).plusWeeks(1)
          )
        )

      case TimeGrouping.Monthly =>
        data.groupBy(entry =>
          TimeRange(
            DateUtils.truncateToMonth(entry.startTime),
            DateUtils.truncateToMonth(entry.startTime).plusMonths(1)
          )
        )

  def search(
      data: List[EnergyData],
      criteria: EnergyData => Boolean
  ): List[EnergyData] =
    data.filter(criteria)
