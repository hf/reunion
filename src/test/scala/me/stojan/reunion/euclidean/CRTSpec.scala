package me.stojan.reunion.euclidean

import me.stojan.reunion.ReunionSpec

import me.stojan.reunion.euclidean.concrete.BigIntEuclidean

class CRTSpec extends ReunionSpec {
  "Chinese Remainder" should "calculate the chinese remainder for { X = 2 mod 3; X = 3 mod 4; X = 1 mod 5 } => X = 11 mod 60" in {
    val congruences = List((BigIntEuclidean(2), BigIntEuclidean(3)),
      (BigIntEuclidean(3), BigIntEuclidean(4)),
      (BigIntEuclidean(1), BigIntEuclidean(5)))

    var solution = ChineseRemainder(congruences).value

    while (solution < 0) {
      solution = (solution + 60) % 60
    }

    solution should be (11)
  }

  it should "throw IllegalArgumentException for a 0 divisor" in {
    val congruences = List((BigIntEuclidean(1), BigIntEuclidean(1)),
      (BigIntEuclidean(1), BigIntEuclidean(0)))
    
    (BigIntEuclidean(0).isZero) should be (true)

    an [java.lang.IllegalArgumentException] should be thrownBy {
      ChineseRemainder(congruences)
    }
  }
  
}
