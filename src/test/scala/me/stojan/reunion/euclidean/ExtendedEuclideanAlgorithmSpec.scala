package me.stojan.reunion.euclidean

import me.stojan.reunion.ReunionSpec

import me.stojan.reunion.euclidean.concrete.BigIntEuclidean

class ExtendedEuclideanAlgorithmSpec extends ReunionSpec {
  "Extended Euclidean Algorithm" should "compute GCD for (2, 3)" in {
    val gcd = ExtendedEuclidean(BigIntEuclidean(2), BigIntEuclidean(3))

    gcd.gcd should be (BigIntEuclidean(1))
  }

  it should "compute GCD for (4, 2)" in {
    val gcd = ExtendedEuclidean(BigIntEuclidean(4), BigIntEuclidean(2))

    gcd.gcd should be (BigIntEuclidean(2))
  }

  it should "compute Bezout's coefficients correctly for (6, 36)" in {
    val gcd = ExtendedEuclidean(BigIntEuclidean(6), BigIntEuclidean(36))

    (gcd.bezout._1 * gcd.input._1) + (gcd.bezout._2 * gcd.input._2)  should be (gcd.gcd)
  }

  it should "not compute anything for (0, X)" in {
    an [ArithmeticException] should be thrownBy {
      ExtendedEuclidean(BigIntEuclidean(0), BigIntEuclidean(1))
    }
  }
}
