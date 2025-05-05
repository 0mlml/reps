package com.reps.services
import scala.collection.immutable._
import com.reps.models._
import com.reps.utils.{DateUtils, ErrorHandling}

import java.io.{File, PrintWriter, FileWriter, BufferedWriter}
import java.nio.file.{Files, Paths}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.io.Source
import scala.util.{Try, Success, Failure}
import scala.jdk.CollectionConverters._

trait FileService:
  def readEnergyData(
    energyType: EnergyType,
    startTime: LocalDateTime,
    endTime: LocalDateTime
  ): Try[List[EnergyData]]
  
  def writeEnergyData(
    energyType: EnergyType,
    data: List[EnergyData]
  ): Try[Unit]
  
  def getFilePath(energyType: EnergyType): String

class FileServiceImpl extends FileService:
  private val dataDirectory = "src/main/resources/data/"
  private val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
  
  def getFilePath(energyType: EnergyType): String = energyType match
    case EnergyType.Solar => s"${dataDirectory}solar_generation.csv"
    case EnergyType.Wind => s"${dataDirectory}wind_generation.csv"
    case EnergyType.Hydro => s"${dataDirectory}hydro_generation.csv"
    case EnergyType.Total => s"${dataDirectory}total_generation.csv"
  
  def readEnergyData(
    energyType: EnergyType,
    startTime: LocalDateTime,
    endTime: LocalDateTime
  ): Try[List[EnergyData]] =
    val filePath = getFilePath(energyType)
    
    Try {
      val source = Source.fromFile(filePath)
      try
        val lines = source.getLines().toList
        
        // Skip header line
        val dataLines = if lines.nonEmpty then lines.tail else List.empty
        
        dataLines.flatMap { line =>
          parseCsvLine(line, energyType)
        }.filter { data =>
          // Filter by time range if specified
          (startTime == null || !data.endTime.isBefore(startTime)) &&
          (endTime == null || !data.startTime.isAfter(endTime))
        }
      finally
        source.close()
    }
  
  def writeEnergyData(
    energyType: EnergyType,
    data: List[EnergyData]
  ): Try[Unit] =
    val filePath = getFilePath(energyType)
    
    Try {
      // Ensure directory exists
      val directory = new File(dataDirectory)
      if !directory.exists() then directory.mkdirs()
      
      val file = new File(filePath)
      val fileExists = file.exists()
      
      val writer = new PrintWriter(new BufferedWriter(new FileWriter(file, fileExists)))
      try
        // Write header if file is new
        if !fileExists then
          val headerLine = getHeaderLine(energyType)
          writer.println(headerLine)
        
        // Write data
        data.foreach { entry =>
          val line = formatEnergyDataToCsv(entry)
          writer.println(line)
        }
        
        ()
      finally
        writer.close()
    }
  
  private def getHeaderLine(energyType: EnergyType): String = energyType match
    case EnergyType.Solar => "\"startTime\";\"endTime\";\"Aurinkovoiman tuotantoennuste - päivitys kerran vuorokaudessa\""
    case EnergyType.Wind => "\"startTime\";\"endTime\";\"Tuulivoimatuotanto - reaaliaikatieto\""
    case EnergyType.Hydro => "\"startTime\";\"endTime\";\"Hydro power production - real time data\""
    case EnergyType.Total => "\"startTime\";\"endTime\";\"Electricity production in Finland\""
  
  private def parseCsvLine(line: String, energyType: EnergyType): Option[EnergyData] =
    if line.trim.isEmpty then return None
    
    // Split by semicolon, handling quoted values
    val parts = line.split(";").map(_.trim.replaceAll("\"", ""))
    
    if parts.length < 3 then return None
    
    Try {
      val startTime = LocalDateTime.parse(parts(0), dateTimeFormatter)
      val endTime = LocalDateTime.parse(parts(1), dateTimeFormatter)
      val value = parts(2).toDouble
      
      EnergyData(energyType, startTime, endTime, value)
    }.toOption
  
  private def formatEnergyDataToCsv(data: EnergyData): String =
    val startTimeStr = data.startTime.format(dateTimeFormatter)
    val endTimeStr = data.endTime.format(dateTimeFormatter)
    
    s"\"$startTimeStr\";\"$endTimeStr\";\"${data.value}\""