package com.reps.utils

import scala.annotation.tailrec

object Statistics:
  def calculateMean(values: List[Double]): Double =
    if values.isEmpty then 0.0
    else values.sum / values.size
  
  def calculateMedian(values: List[Double]): Double =
    if values.isEmpty then 0.0
    else
      val sorted = values.sorted
      val mid = sorted.size / 2
      if sorted.size % 2 == 0 then
        (sorted(mid - 1) + sorted(mid)) / 2.0
      else
        sorted(mid)
  
  def calculateMode(values: List[Double]): Option[Double] =
    if values.isEmpty then None
    else
      val grouped = values.groupBy(identity).view.mapValues(_.size).toList
      val maxCount = grouped.map(_._2).max
      
      if maxCount == 1 then None 
      else
        val modes = grouped.filter(_._2 == maxCount).map(_._1)
        modes.headOption
  
  def calculateRange(values: List[Double]): Double =
    if values.isEmpty then 0.0
    else values.max - values.min
  
  def calculateMidrange(values: List[Double]): Double =
    if values.isEmpty then 0.0
    else (values.max + values.min) / 2.0
  
  def recursiveMean(values: List[Double]): Double =
    @tailrec
    def sumHelper(list: List[Double], acc: Double): Double =
      list match
        case Nil => acc
        case head :: tail => sumHelper(tail, acc + head)
    
    if values.isEmpty then 0.0
    else sumHelper(values, 0.0) / values.size
  
  def recursiveMedian(values: List[Double]): Double =
    if values.isEmpty then 0.0
    else
      val sorted = sortList(values)
      val mid = sorted.size / 2
      if sorted.size % 2 == 0 then
        (sorted(mid - 1) + sorted(mid)) / 2.0
      else
        sorted(mid)
  
  private def sortList(list: List[Double]): List[Double] =
    @tailrec
    def merge(left: List[Double], right: List[Double], acc: List[Double]): List[Double] =
      (left, right) match
        case (Nil, _) => acc.reverse ++ right
        case (_, Nil) => acc.reverse ++ left
        case (lHead :: lTail, rHead :: rTail) =>
          if lHead <= rHead then
            merge(lTail, right, lHead :: acc)
          else
            merge(left, rTail, rHead :: acc)
    
    if list.length <= 1 then list
    else
      val (left, right) = list.splitAt(list.length / 2)
      merge(sortList(left), sortList(right), Nil)