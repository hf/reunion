package me.stojan.reunion.euclidean

/**
 * Functions for the Extended Euclidean Algorithm.
 */
object ExtendedEuclidean {
  /**
   * Computes the Extended Euclidean Algorithm for the `a` and `b` Euclidean
   * domain elements.
   */
  def apply[V](a: Euclidean[V], b: Euclidean[V]): GCD[Euclidean[V]] = {
    // ._1 is "new", ._2 is "old"

    var s = (a - a, a / a)
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

