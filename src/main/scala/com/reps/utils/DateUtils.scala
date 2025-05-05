package com.reps.utils
import scala.collection.immutable._
import java.time.{LocalDateTime, DayOfWeek}
import java.time.temporal.{TemporalAdjusters, ChronoUnit}

object DateUtils:
  // Define the specific date and time
  val specificDateTime = LocalDateTime.of(2025, 3, 31, 18, 0)

  def truncateToHour(dateTime: LocalDateTime): LocalDateTime =
    dateTime.truncatedTo(ChronoUnit.HOURS)

  def truncateToDay(dateTime: LocalDateTime): LocalDateTime =
    dateTime.truncatedTo(ChronoUnit.DAYS)

  def truncateToWeek(dateTime: LocalDateTime): LocalDateTime =
    // Fix: Use apply method instead of with
    val truncated = dateTime.truncatedTo(ChronoUnit.DAYS)
    truncated.`with`(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY))

  def truncateToMonth(dateTime: LocalDateTime): LocalDateTime =
    dateTime.withDayOfMonth(1).truncatedTo(ChronoUnit.DAYS)

  def parseDateTime(dateTimeStr: String): Option[LocalDateTime] =
    try
      Some(LocalDateTime.parse(dateTimeStr))
    catch
      case _: Exception => None

  def startOfDay(): LocalDateTime = specificDateTime.truncatedTo(ChronoUnit.DAYS)
  def endOfDay(): LocalDateTime = startOfDay().plusDays(1).minusNanos(1)

  // Get start and end of current week
  def startOfWeek(): LocalDateTime = {
    val now = specificDateTime.truncatedTo(ChronoUnit.DAYS)
    now.`with`(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY))
  }

  def endOfWeek(): LocalDateTime = startOfWeek().plusWeeks(1).minusNanos(1)

  // Get start and end of current month
  def startOfMonth(): LocalDateTime = {
    specificDateTime.withDayOfMonth(1).truncatedTo(ChronoUnit.DAYS)
  }

  def endOfMonth(): LocalDateTime = {
    startOfMonth().plusMonths(1).minusNanos(1)
  }
