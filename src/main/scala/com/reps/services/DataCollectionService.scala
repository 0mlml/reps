package com.reps.services
import scala.collection.immutable._
import com.reps.models._
import com.reps.utils.ErrorHandling._

import java.time.LocalDateTime
import scala.util.Try

trait DataCollectionService:
  def collectData(energyType: EnergyType, startTime: LocalDateTime, endTime: LocalDateTime): Try[List[EnergyData]]
  def storeData(data: List[EnergyData]): Try[Unit]

class DataCollectionServiceImpl(fileService: FileService) extends DataCollectionService:
  def collectData(energyType: EnergyType, startTime: LocalDateTime, endTime: LocalDateTime): Try[List[EnergyData]] =
    fileService.readEnergyData(energyType, startTime, endTime)
      .map(filterDataByTimeRange(_, startTime, endTime))
  
  def storeData(data: List[EnergyData]): Try[Unit] =
    val groupedData = data.groupBy(_.energyType)
    
    val results = groupedData.map { case (energyType, typeData) =>
      fileService.writeEnergyData(energyType, typeData)
    }
    
    sequence(results.toList).map(_ => ())
  
  private def filterDataByTimeRange(data: List[EnergyData], start: LocalDateTime, end: LocalDateTime): List[EnergyData] =
    data.filter(entry => 
      !entry.startTime.isBefore(start) && !entry.endTime.isAfter(end)
    )