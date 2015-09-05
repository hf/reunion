package me.stojan.reunion.euclidean

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
