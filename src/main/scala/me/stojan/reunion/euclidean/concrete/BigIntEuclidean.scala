package me.stojan.reunion.euclidean.concrete

import me.stojan.reunion.euclidean.{ Euclidean, EuclideanDescriptor }

private object BigIntEuclideanDescriptor extends EuclideanDescriptor[BigInt] {
  override val one: Euclidean[BigInt] = obtain(1)
  override val zero: Euclidean[BigInt] = obtain(0)

  override def obtain(value: BigInt): Euclidean[BigInt] = BigIntEuclidean(value)
}

/**
 * BigInt as a Euclidean domain.
 */
case class BigIntEuclidean(value: BigInt) extends Euclidean[BigInt] {
  override val descriptor: EuclideanDescriptor[BigInt] = BigIntEuclideanDescriptor

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
