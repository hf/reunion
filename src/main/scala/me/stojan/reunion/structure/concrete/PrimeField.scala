package me.stojan.reunion.structure.concrete

import me.stojan.reunion.structure.Field

import me.stojan.reunion.euclidean.{ Euclidean, ExtendedEuclidean }
import me.stojan.reunion.euclidean.concrete.BigIntEuclidean

/**
 * A prime finite field.
 */
case class PrimeField(prime: BigInt, value: BigInt) extends Field[BigInt] {
  override def +(f: Field[BigInt]): Field[BigInt] = (value + f.value) % prime
  override def *(f: Field[BigInt]): Field[BigInt] = (value * f.value) % prime
  override def unary_-(): Field[BigInt] = (prime - value)

  /**
   * Uses `ExtendedEuclidean` to calculate the multiplicative inverse, so this
   * is a slow operation.
   */
  override def unary_~(): Field[BigInt] = (ExtendedEuclidean(value, prime).bezout._1 + prime) % prime

  private implicit def bigIntToField(i: BigInt): Field[BigInt] = PrimeField(prime, i)
  private implicit def bigIntToEuclidean(i: BigInt): Euclidean[BigInt] = BigIntEuclidean(i)
  private implicit def euclideanToBigInt(e: Euclidean[BigInt]): BigInt = e.value
}
