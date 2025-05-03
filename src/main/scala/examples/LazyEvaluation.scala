package examples

import scala.annotation.tailrec

/**
 * This file demonstrates the concept of lazy evaluation in Scala 3.
 * Lazy evaluation is a strategy that delays the evaluation of an expression
 * until its value is needed, which can lead to performance improvements and
 * the ability to work with infinite data structures.
 */
object LazyEvaluation:
  // Part 1: Lazy Values
  
  // A non-lazy value is evaluated immediately when defined
  val eagerValue: Int = {
    println("Computing eager value...")
    42
  }
  
  // A lazy value is only evaluated when first accessed
  lazy val lazyValue: Int = {
    println("Computing lazy value...")
    42
  }
  
  // Part 2: Infinite Sequences with Lazy Evaluation
  def infiniteStream(start: Int): LazyList[Int] =
    start #:: infiniteStream(start + 1)
  
  // This function returns the first n elements of the stream
  def takeFirstN(stream: LazyList[Int], n: Int): List[Int] =
    stream.take(n).toList
  
  // Part 3: Lazy Filtering and Mapping
  
  // Define a lazily evaluated filter function
  def lazyFilter[A](list: List[A], predicate: A => Boolean): LazyList[A] =
    list match
      case Nil => LazyList.empty
      case head :: tail =>
        if predicate(head) then
          head #:: lazyFilter(tail, predicate)
        else
          lazyFilter(tail, predicate)
  
  // Define a lazily evaluated map function
  def lazyMap[A, B](list: List[A], f: A => B): LazyList[B] =
    list match
      case Nil => LazyList.empty
      case head :: tail => f(head) #:: lazyMap(tail, f)
  
  // Part 4: Demonstrating Benefits of Lazy Evaluation with Energy Data Processing
  
  // Simulate a large dataset of energy readings
  def generateEnergyReadings(count: Int): LazyList[Double] =
    LazyList.iterate(0.0)(_ + 1.0).take(count).map { base =>
      // Create some variation in the values
      base + math.sin(base) * 10.0
    }
  
  // Process only necessary data using lazy evaluation
  def findFirstReadingAboveThreshold(
    readings: LazyList[Double],
    threshold: Double
  ): Option[Double] =
    readings.find(_ > threshold)
  
  // Without lazy evaluation, this would process the entire dataset
  // With lazy evaluation, it stops as soon as a match is found
  
  // Part 5: Memory Efficiency Example for REPS
  
  // Process large datasets of energy production records efficiently
  case class EnergyRecord(timestamp: Long, value: Double)
  
  def processLargeEnergyDataset(
    getData: => LazyList[EnergyRecord],
    processingFunction: EnergyRecord => Boolean
  ): Int =
    // Count records that match the processing function
    // The dataset is evaluated lazily, so we don't load everything into memory
    @tailrec
    def countMatching(stream: LazyList[EnergyRecord], acc: Int): Int =
      if stream.isEmpty then acc
      else countMatching(stream.tail, acc + (if processingFunction(stream.head) then 1 else 0))
    
    countMatching(getData, 0)
  
  def main(args: Array[String]): Unit =
    println("1. Demonstrating lazy values:")
    println(s"Eager value is already computed: $eagerValue")
    println("About to access lazy value...")
    println(s"Lazy value computed only when accessed: $lazyValue")
    
    println("\n2. Working with infinite streams:")
    val numbers = infiniteStream(1)
    println(s"First 10 numbers: ${takeFirstN(numbers, 10)}")
    
    println("\n3. Lazy filtering and mapping:")
    val originalList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val evenNumbers = lazyFilter(originalList, (n: Int) => n % 2 == 0)
    val squaredNumbers = lazyMap(originalList, (n: Int) => n * n)
    
    println(s"Even numbers: ${evenNumbers.toList}")
    println(s"Squared numbers: ${squaredNumbers.toList}")
    
    println("\n4. Energy data processing with lazy evaluation:")
    val energyReadings = generateEnergyReadings(1000000) // Simulate 1 million readings
    println("Generated energy readings (lazily)")
    
    val startTime = System.currentTimeMillis()
    val firstHighReading = findFirstReadingAboveThreshold(energyReadings, 15.0)
    val endTime = System.currentTimeMillis()
    
    println(s"Found reading above threshold: $firstHighReading")
    println(s"Time taken: ${endTime - startTime}ms")
    println("Notice how quickly this completes despite the large dataset size")
    
    println("\n5. Memory efficiency with large datasets:")
    // Simulate processing 10 million records without loading them all into memory
    val largeDataset = LazyList.iterate(EnergyRecord(0, 0.0))(record => 
      EnergyRecord(record.timestamp + 1, math.random() * 100)
    ).take(10000000)
    
    val anomalyCount = processLargeEnergyDataset(
      largeDataset,
      record => record.value > 90.0 // Looking for anomalously high values
    )
    
    println(s"Found $anomalyCount anomalies in the dataset")
    println("Processing completed efficiently using lazy evaluation")