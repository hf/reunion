package me.stojan.reunion.euclidean.concrete

import me.stojan.reunion.euclidean.Euclidean

/**
 * BigInt as a Euclidean domain.
 */
case class BigIntEuclidean(value: BigInt) extends Euclidean[BigInt] {
  override def +(e: Euclidean[BigInt]): Euclidean[BigInt] = value + e.value
  override def -(e: Euclidean[BigInt]): Euclidean[BigInt] = value - e.value
  override def *(e: Euclidean[BigInt]): Euclidean[BigInt] = value * e.value
  override def /(e: Euclidean[BigInt]): Euclidean[BigInt] = value / e.value
  override def unary_-(): Euclidean[BigInt] = -value

  override val isZero: Boolean = value == 0
  override val isOne:  Boolean = value == 1

  override def >(e: Euclidean[BigInt]): Boolean = value > e.value
  override def <(e: Euclidean[BigInt]): Boolean = value < e.value

  private implicit def bigIntToEuclidean(v: BigInt): Euclidean[BigInt] = BigIntEuclidean(v)
}
