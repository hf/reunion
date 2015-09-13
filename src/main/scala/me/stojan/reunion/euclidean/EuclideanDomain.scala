package me.stojan.reunion.euclidean

import me.stojan.reunion.structure.Ring

/**
 * A generic Euclidean domain element.
 */
trait Euclidean[V] {
  /**
   * Backing value of this element.
   */
  def value: V

  /**
   * Addition with another element in the same domain.
   */
  def +(e: Euclidean[V]): Euclidean[V]

  /**
   * Subtraction with another element in the same domain.
   */
  def -(e: Euclidean[V]): Euclidean[V]

  /**
   * Multiplication with another element in the same domain.
   */
  def *(e: Euclidean[V]): Euclidean[V]

  /**
   * Division with another element in the same domain. Division by the zero
   * element is undefined. It will most likely result in a
   * `java.lang.ArithmeticException` from the use of arithmetic on the generic
   * type.
   */
  def /(e: Euclidean[V]): Euclidean[V]

  /**
   * Whether this element is the zero element, i.e. zero * anything = zero.
   */
  def isZero: Boolean

  /**
   * Whether this element is not the zero element.
   */
  def nonZero: Boolean = !isZero
}

/**
 * Holds implicits for convering `Euclidean`s to `Ring`s.
 */
object EuclideanConversions {
  private case class RingEuclidean[V](euclidean: Euclidean[V]) extends Ring[V] {
    override def value: V = euclidean.value
    override def unary_-(): Ring[V] = -euclidean
    override def +(r: Ring[V]): Ring[V] = euclidean + r
    override def -(r: Ring[V]): Ring[V] = euclidean - r
    override def *(r: Ring[V]): Ring[V] = euclidean * r
  }

  implicit def euclideanToRing[V](e: Euclidean[V]): Ring[V] = RingEuclidean[V](e)
}
