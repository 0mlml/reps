package com.reps.models

import java.time.LocalDateTime

case class TimeRange(startTime: LocalDateTime, endTime: LocalDateTime)

enum TimeGrouping:
  case Hourly, Daily, Weekly, Monthly

case class Statistics(
  energyType: EnergyType,
  timeRange: TimeRange,
  grouping: TimeGrouping,
  mean: Double,
  median: Double,
  mode: Option[Double],
  range: Double,
  midrange: Double
)