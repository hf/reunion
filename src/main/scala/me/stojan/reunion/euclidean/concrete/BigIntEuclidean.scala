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

  override val isZero: Boolean = value == 0

  private implicit def bigIntToEuclidean(v: BigInt): Euclidean[BigInt] = BigIntEuclidean(v)
}
