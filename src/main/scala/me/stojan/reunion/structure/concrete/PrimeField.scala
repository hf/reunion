package me.stojan.reunion.structure.concrete

import me.stojan.reunion.structure.{ Field, FieldDescriptor }

import me.stojan.reunion.euclidean.{ Euclidean, ExtendedEuclidean }
import me.stojan.reunion.euclidean.concrete.BigIntEuclidean

object PrimeField {
  /**
   * Creates a new element in the finite field GF(prime).
   */
  def apply(prime: BigInt, value: BigInt): PrimeField = new PrimeField(prime, value % prime)
}

private case class PrimeFieldDescriptor(prime: BigInt) extends FieldDescriptor[BigInt] {
  override lazy val one: Field[BigInt] = obtain(1)
  override lazy val zero: Field[BigInt] = obtain(0)

  override def obtain(value: BigInt): Field[BigInt] = PrimeField(prime, value)
}

/**
 * A prime finite field.
 *
 * Construction should be done such that `value < prime`.
 */
class PrimeField protected (val prime: BigInt, val value: BigInt) extends Field[BigInt] {
  override lazy val descriptor: FieldDescriptor[BigInt] = PrimeFieldDescriptor(prime)

  override val isZero: Boolean = value == 0
  override val isOne:  Boolean = value == 1

  override def +(f: Field[BigInt]): Field[BigInt] = (value + f.value) % prime
  override def *(f: Field[BigInt]): Field[BigInt] = (value * f.value) % prime
  override def unary_-(): Field[BigInt] = (prime - value)

  override def >(f: Field[BigInt]): Boolean = value > f.value
  override def <(f: Field[BigInt]): Boolean = value < f.value

  /**
   * Uses `ExtendedEuclidean` to calculate the multiplicative inverse, so this
   * is a slow operation.
   */
  override def unary_~(): Field[BigInt] =
    if (isZero) {
      throw new java.lang.UnsupportedOperationException("Multiplicative inverse not defined for the zero element: " + this)
    } else {
      // value will always be <= prime, so bezout._1 + prime is the inverse
      (ExtendedEuclidean(value, prime).bezout._1 + prime) % prime
    }

  private implicit def bigIntToField(i: BigInt): Field[BigInt] = PrimeField(prime, i)
  private implicit def bigIntToEuclidean(i: BigInt): Euclidean[BigInt] = BigIntEuclidean(i)
  private implicit def euclideanToBigInt(e: Euclidean[BigInt]): BigInt = e.value

  override def hashCode: Int = prime.hashCode ^ value.hashCode

  override def equals(a: Any): Boolean = a match {
    case p: PrimeField => p.prime == prime && p.value == value
    case _ => false
  }

  override def toString: String = "PrimeField(" + prime + ", " + value + ")"
}
