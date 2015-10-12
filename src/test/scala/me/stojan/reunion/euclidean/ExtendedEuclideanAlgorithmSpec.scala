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

  it should "throw IllegalArgumentException for non-same domain elements" in {
    val a = mock[Euclidean[BigInt]]

    (a.descriptor _).expects().returning(mock[EuclideanDescriptor[BigInt]])

    val b = BigIntEuclidean(10)

    an [IllegalArgumentException] should be thrownBy {
      ExtendedEuclidean(a, b)
    }
  }

  it should "compute GCD(0, 0)" in {
    val gcd = ExtendedEuclidean(BigIntEuclidean(0), BigIntEuclidean(0))

    (gcd.gcd) should be (BigIntEuclidean(0))

    (gcd.bezout._1 * gcd.input._1) + (gcd.bezout._2 * gcd.input._2) should be (gcd.gcd)

    (gcd.quotients._1) should be (BigIntEuclidean(0))
    (gcd.quotients._2) should be (BigIntEuclidean(0))

    (gcd.bezout._1) should be (BigIntEuclidean(0))
    (gcd.bezout._2) should be (BigIntEuclidean(0))
  }

  it should "compute GCD(0, 1)" in {
    val gcd = ExtendedEuclidean(BigIntEuclidean(0), BigIntEuclidean(1))

    (gcd.gcd) should be (BigIntEuclidean(1))

    (gcd.input._1) should be (BigIntEuclidean(0))
    (gcd.input._2) should be (BigIntEuclidean(1))

    (gcd.bezout._1 * gcd.input._1) + (gcd.bezout._2 * gcd.input._2) should be (gcd.gcd)

    (gcd.bezout._1) should be (BigIntEuclidean(0))
    (gcd.bezout._2) should be (BigIntEuclidean(1))

    (gcd.quotients._1) should be (BigIntEuclidean(0))
    (gcd.quotients._2) should be (BigIntEuclidean(1))
  }

  it should "compute GCD(1, 0) as if it were GCD(0, 1)" in {
    val gcdA = ExtendedEuclidean(BigIntEuclidean(1), BigIntEuclidean(0))
    val gcdB = ExtendedEuclidean(BigIntEuclidean(0), BigIntEuclidean(1))

    (gcdA) should be (gcdB)
  }
}
