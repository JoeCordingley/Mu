package com.joecordingley.mu

import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Order
import cats.data.Ior

trait Evaluate[A] {
  def children(a: A): List[A]
  def value(a: A): Double
}

trait Evaluate2[A, B] {
  def children(a: A): Stream[A]
  def value(a: A): B
}

object Evaluate2 {
  implicit class Evaluate2Ops[A, B](val a: A) extends AnyVal {
    def children(implicit e: Evaluate2[A, B]) = e.children(a)
    def value(implicit e: Evaluate2[A, B]) = e.value(a)
  }
}
object Evaluate {
  implicit class EvaluateOps[A](val a: A) extends AnyVal {
    def children(implicit e: Evaluate[A]) = e.children(a)
    def value(implicit e: Evaluate[A]) = e.value(a)
  }
}

object MinMax {
  import Evaluate._

  def minMax[A: Evaluate](a: A, max: Boolean, depth: Int): Double =
    if (depth == 0) a.value
    else
      a.children match {
        case Nil => a.value
        case x :: xs => {
          val values =
            NonEmptyList.of(x, xs: _*).map(minMax(_, !max, depth - 1))
          if (max) values.maximum else values.minimum
        }
      }

  def alphaBeta[A: Evaluate](a: A, max: Boolean, depth: Int): Double = {

    def inner(
        a: A,
        max: Boolean,
        depth: Int,
        alpha: Double,
        beta: Double): Double = {
      def prune(
          value: Double,
          as: List[A],
          alpha: Double,
          beta: Double): Double =
        if (alpha >= beta) value
        else
          as match {
            case Nil => value
            case x :: xs if max => {
              val newValue = value max inner(x, false, depth - 1, alpha, beta)
              prune(newValue, xs, alpha max newValue, beta)
            }
            case x :: xs => {
              val newValue = value min inner(x, true, depth - 1, alpha, beta)
              prune(newValue, xs, alpha, beta min newValue)
            }
          }

      if (depth == 0) a.value
      else
        a.children match {
          case Nil => a.value
          case xs if max => prune(Double.MinValue, xs, alpha, beta)
          case xs => prune(Double.MaxValue, xs, alpha, beta)
        }
    }
    inner(a, max, depth, Double.MinValue, Double.MaxValue)
  }

  def alphaBeta2[A: Evaluate](a: A, max: Boolean, depth: Int): Double = {
    case class AlphaBeta(alpha: Double, beta: Double)
    val init = AlphaBeta(Double.MinValue, Double.MaxValue)
    def inner(a: A, max: Boolean, depth: Int, alphaBeta: AlphaBeta): Double =
      if (depth == 0) a.value
      else {
        lazy val s: Stream[(Double, AlphaBeta)] = a.children.toStream
          .zip {
            (if (max) Double.MinValue else Double.MaxValue, alphaBeta) #:: s
          }
          .map {
            case (child, (value, ab @ AlphaBeta(alpha, beta))) => {
              val newValue = inner(child, !max, depth - 1, ab)
              if (max) (value max newValue, AlphaBeta(newValue max alpha, beta))
              else (value min newValue, AlphaBeta(alpha, newValue min beta))
            }
          }
        lazy val streams: Stream[Stream[(Double, AlphaBeta)]] = s #:: streams
          .takeWhile(_.nonEmpty)
          .map(_.tail)
        streams
          .collectFirst {
            case (value, AlphaBeta(alpha, beta)) #:: _ if alpha >= beta => value
            case (value, _) #:: Stream.Empty => value
          }
          .getOrElse(a.value)
      }
    inner(a, max, depth, init)
  }
//  def alphaBeta3[A, B](a: A, max: Boolean, depth: Int)(implicit evaluate: Evaluate2[A, B]): B = {
//    case class AlphaBeta(alpha: Option[B], beta: Option[B])
//    val init = AlphaBeta(None, None)
//    def inner(a: A, max: Boolean, depth: Int, alphaBeta: AlphaBeta): B =
//      if (depth == 0) a.value
//      else {
//        lazy val s: Stream[(Option[B], AlphaBeta)] = a.children.toStream
//          .zip {
//            (None, alphaBeta) #:: s
//          }
//          .map {
//            case (child, (maybeValue, ab @ AlphaBeta(alpha, beta))) => {
//              val newValue = inner(child, !max, depth - 1, ab)
//              if (max) (Some(maxOption(maybeValue, newValue)), AlphaBeta(Some(maxOption(alpha, newValue)), beta))
//              else (Some(minOption(maybeValue, newValue)), AlphaBeta(alpha, Some(minOption(beta, newValue)))
//            }
//          }
//        lazy val streams: Stream[Stream[(Double, AlphaBeta)]] = s #:: streams
//          .takeWhile(_.nonEmpty)
//          .map(_.tail)
//        streams
//          .collectFirst {
//            case (value, AlphaBeta(alpha, beta)) #:: _ if alpha >= beta => value
//            case (value, _) #:: Stream.Empty => value
//          }
//          .getOrElse(a.value)
//      }
//    inner(a, max, depth, init)
//  }

}
object MinMax2 {
  import Evaluate2._

  private def minOption[A: Order](m: Option[A], a: A) = m.fold(a)(a min _)
  private def maxOption[A: Order](m: Option[A], a: A) = m.fold(a)(a max _)

  def alphaBeta3[A, B](a: A, max: Boolean, depth: Int)(
      implicit e: Evaluate2[A, B],
      o: Order[B]): B = {
    case class AlphaBeta(alpha: Option[B], beta: Option[B])
    val init = AlphaBeta(None, None)
    def inner(a: A, max: Boolean, depth: Int, alphaBeta: AlphaBeta): B =
      if (depth == 0) a.value
      else {
        lazy val s: Stream[(B, AlphaBeta)] = a.children
          .zip {
            (None, alphaBeta) #:: s.map {
              case (b, ab) => (Some(b), ab)
            }
          }
          .map {
            case (child, (maybeValue, ab @ AlphaBeta(alpha, beta))) => {
              val newValue = inner(child, !max, depth - 1, ab)
              if (max)
                (
                  maxOption(maybeValue, newValue),
                  AlphaBeta(Some(maxOption(alpha, newValue)), beta))
              else
                (
                  minOption(maybeValue, newValue),
                  AlphaBeta(alpha, Some(minOption(beta, newValue))))
            }
          }
        lazy val streams: Stream[Stream[(B, AlphaBeta)]] = s #:: streams
          .takeWhile(_.nonEmpty)
          .map(_.tail)
        streams
          .collectFirst {
            case (value, AlphaBeta(Some(alpha), Some(beta))) #:: _
                if alpha >= beta =>
              value
            case (value, _) #:: Stream.Empty => value
          }
          .getOrElse(a.value)
      }
    inner(a, max, depth, init)
  }
}
