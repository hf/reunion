package me.stojan.reunion.euclidean

/**
 * Functions for the Extended Euclidean Algorithm.
 */
object ExtendedEuclidean {
  /**
   * Computes the Extended Euclidean Algorithm for the `a` and `b` Euclidean
   * domain elements.
   *
   * If `a` is less than `b` then the GCD of (b, a) will be computed, but it will return values as if
   * (a, b) was computed. This is done to have consistent (and not non-negative) GCD values.
   */
  def apply[V](a: Euclidean[V], b: Euclidean[V]): GCD[Euclidean[V]] = {
    if (a.descriptor != b.descriptor) {
      throw new java.lang.IllegalArgumentException("a, b do not belong to the same euclidean domain")
    }

    val descriptor = a.descriptor

    if (a > b) {
      return apply(b, a)
    }

    if (b.isZero) {
      val zero = descriptor.zero

      return GCD(input = (a, b), bezout = (zero, zero), gcd = zero, quotients = (zero, zero))
    }

    val zero = descriptor.zero
    val one = descriptor.one

    // ._1 is "new", ._2 is "old"

    var s = (zero, one)
    var t = (s._2, s._1)
    var r = (b, a)

    while (r._1.nonZero) {
      val quotient = r._2 / r._1
      r = (r._2 - (quotient * r._1), r._1)
      s = (s._2 - (quotient * s._1), s._1)
      t = (t._2 - (quotient * t._1), t._1)
    }

    GCD(input = (a, b), bezout = (s._2, t._2), gcd = r._2, quotients = (t._1, s._1))
  }
}

/**
 * Holds results from running the Extended Euclidean Algorithm.
 */
case class GCD[X](
  /**
   * The pair of inputs.
   */
  input: (X, X),
  /**
   * Bezout coefficients.
   */
  bezout: (X, X),
  /**
   * Greatest Common Divisor.
   */
  gcd: X,
  /**
   * Quotients by the GCD.
   */
  quotients: (X, X))

