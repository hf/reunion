package me.stojan.reunion.euclidean

/**
 * Functions concerning the Chinese Remainder Theorem, as a method of solving
 * congruence relations in a Euclidean domain.
 *
 * http://mathworld.wolfram.com/ChineseRemainderTheorem.html
 */
object ChineseRemainder {
  /**
   * Computes the Chinese remainder for a variable X. The first of the
   * `congruences` is the remainder, while the second is the divisor.
   *
   * It returns a solution to the variable X. All other solutions are congruent
   * to the LCM of the divisors.
   *
   * A requirement for this algorithm is that the divisors be pairwise coprime.
   */
  def apply[V](congruences: Seq[(Euclidean[V], Euclidean[V])]): Euclidean[V] = {
    val remainders = congruences.map(_._1)
    val divisors = congruences.map(_._2)

    val N = divisors.tail.fold(divisors.head) { (a, i) => a * i }

    val es = divisors.map { (d) =>
      val Nd = N / d

      ExtendedEuclidean(d, Nd).bezout._2 * Nd
    }

    val res = remainders.zip(es).map((i) => i._1 * i._2)

    res.tail.fold(res.head) { (a, i) => a + i }
  }
}
