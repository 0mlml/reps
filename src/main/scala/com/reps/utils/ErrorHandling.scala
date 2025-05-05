package com.reps.utils
import scala.collection.immutable._
import scala.util.{Try, Success, Failure}

object ErrorHandling:
  def sequence[A](list: List[Option[A]]): Option[List[A]] =
    list match
      case Nil => Some(Nil)
      case head :: tail => 
        head.flatMap(h => sequence(tail).map(t => h :: t))
  
  def sequence[A](list: List[Try[A]]): Try[List[A]] =
    list match
      case Nil => Success(Nil)
      case head :: tail =>
        head.flatMap(h => sequence(tail).map(t => h :: t))
  
  def mapOption[A, B](opt: Option[A], f: A => B): Option[B] =
    opt.map(f)
  
  def mapTry[A, B](t: Try[A], f: A => B): Try[B] =
    t.map(f)
  
  def eitherFromTry[A](t: Try[A]): Either[Throwable, A] =
    t match
      case Success(value) => Right(value)
      case Failure(exception) => Left(exception)
  
  def handleErrors[A](t: Try[A], defaultValue: => A): A =
    t.getOrElse(defaultValue)
  
  def fold[A, B, C](either: Either[A, B], ifLeft: A => C, ifRight: B => C): C =
    either match
      case Left(a) => ifLeft(a)
      case Right(b) => ifRight(b)