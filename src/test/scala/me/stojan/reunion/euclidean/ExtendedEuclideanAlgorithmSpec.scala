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

    (gcd.bezout._1 * gcd.input._1) + (gcd.bezout._2 * gcd.input._2) should be (gcd.gcd)

    (gcd.quotients._1 * gcd.gcd) should (be (gcd.input._1) or be (-gcd.input._1))
    (gcd.quotients._2 * gcd.gcd) should (be (gcd.input._2) or be (-gcd.input._2))
  }

  it should "not throw IllegalArgumentException for GCD(X, 0) = X" in {
    noException should be thrownBy {
      val gcd = ExtendedEuclidean(BigIntEuclidean(1), BigIntEuclidean(0))

      gcd.gcd should be (BigIntEuclidean(1))

      ((gcd.input._1 * gcd.bezout._1) + (gcd.input._2 * gcd.bezout._2)) should be (gcd.gcd)

      (gcd.quotients._1 * gcd.gcd) should be (gcd.input._1)
      (gcd.quotients._2 * gcd.gcd) should be (gcd.input._2)
    }
  }

  it should "throw IllegalArgumentException for GCD(0, 0)" in {
    an [java.lang.IllegalArgumentException] should be thrownBy {
      ExtendedEuclidean(BigIntEuclidean(0), BigIntEuclidean(0))
    }
  }
}
