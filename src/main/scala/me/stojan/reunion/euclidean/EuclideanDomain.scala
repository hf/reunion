package me.stojan.reunion.euclidean

import me.stojan.reunion.structure.Ring

/**
 * Describes a euclidean domain.
 */
trait EuclideanDescriptor[V] {
  /**
   * Converts a primitive value to a euclidean domain element with this domain.
   */
  def obtain(value: V): Euclidean[V]

  /**
   * Multiplicative identity element.
   */
  def one: Euclidean[V]

  /**
   * Additive identity element.
   */
  def zero: Euclidean[V]
}

/**
 * A generic Euclidean domain element.
 */
trait Euclidean[V] {
  /**
   * This element's descriptor.
   */
  def descriptor: EuclideanDescriptor[V]

  /**
   * Backing value of this element.
   */
  def value: V

  /**
   * Addition with another element in the same domain.
   */
  def +(e: Euclidean[V]): Euclidean[V]

  /**
   * Additive inverse of this element.
   */
  def unary_-(): Euclidean[V]

  /**
   * Subtraction with another element in the same domain.
   */
  def -(e: Euclidean[V]): Euclidean[V] = this + (-e)

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

  /**
   * Whether this element is the multiplicative identity element.
   */
  def isOne: Boolean

  /**
   * Whether this element is not the multiplicative identity element.
   */
  def nonOne: Boolean = !isOne

  /**
   * Whether this element is greater than `e`.
   */
  def >(e: Euclidean[V]): Boolean

  /**
   * Whether this element is greater than or equal to `e`.
   */
  def >=(e: Euclidean[V]): Boolean = (this > e) || (this == e)

  /**
   * Whether this element is less than `e`.
   */
  def <(e: Euclidean[V]): Boolean

  /**
   * Whether this element is less than or equal to `e`.
   */
  def <=(e: Euclidean[V]): Boolean = (this < e) || (this == e)
}
