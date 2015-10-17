package me.stojan.reunion.structure.concrete

import me.stojan.reunion.structure.{ Field, FieldDescriptor }

import me.stojan.reunion.euclidean.{ Euclidean, ExtendedEuclidean }
import me.stojan.reunion.euclidean.concrete.BigIntEuclidean

private case class PrimeFieldDescriptor(prime: BigInt) extends FieldDescriptor[BigInt] {
  override lazy val one: Field[BigInt] = obtain(1)
  override lazy val zero: Field[BigInt] = obtain(0)

  override def obtain(value: BigInt): Field[BigInt] = PrimeField(this, value % prime)
}

/**
 * A prime finite field.
 *
 * Construction should be done such that `value < prime`.
 */
case class PrimeField(descriptor: FieldDescriptor[BigInt], val value: BigInt) extends Field[BigInt] {
  override val isZero: Boolean = value == 0
  override val isOne:  Boolean = value == 1

  override def +(f: Field[BigInt]): Field[BigInt] = descriptor.obtain(value + f.value)
  override def *(f: Field[BigInt]): Field[BigInt] = descriptor.obtain(value * f.value)
  override def unary_-(): Field[BigInt] = descriptor.obtain(prime - value)

  override def >(f: Field[BigInt]): Boolean = value > f.value
  override def <(f: Field[BigInt]): Boolean = value < f.value

  private val prime: BigInt = descriptor.asInstanceOf[PrimeFieldDescriptor].prime

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

  private implicit def bigIntToField(i: BigInt): Field[BigInt] = descriptor.obtain(i)
  private implicit def bigIntToEuclidean(i: BigInt): Euclidean[BigInt] = BigIntEuclidean(i)
  private implicit def euclideanToBigInt(e: Euclidean[BigInt]): BigInt = e.value
}

object PrimeField {

  def descriptor(prime: BigInt): FieldDescriptor[BigInt] = PrimeFieldDescriptor(prime)

  def apply(value: BigInt)(implicit primeFieldDescriptor: FieldDescriptor[BigInt]): Field[BigInt] = primeFieldDescriptor.obtain(value)
}
