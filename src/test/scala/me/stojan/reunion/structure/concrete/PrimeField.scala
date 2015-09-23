package me.stojan.reunion.structure.concrete

import me.stojan.reunion.ReunionSpec

class PrimeFieldSpec extends ReunionSpec {

  "PrimeField" should "calculate 1 + 10 == 0 in GF(11)" in {
    val a = PrimeField(11, 1)
    val b = PrimeField(11, 10)

    (a + b) should be (PrimeField(11, 0))
  }

  it should "assign PF(11, 12) to be PF(11, 1)" in {
    PrimeField(11, 12) should be (PrimeField(11, 1))
  }

  it should "calculate 1 * 1 == 1 in GF(11)" in {
    val a = PrimeField(11, 1)

    (a * a) should be (PrimeField(11, 1))
  }

  it should "calculate 1 / 1 == 1 in GF(11)" in {
    val a = PrimeField(11, 1)

    (a / a) should be (PrimeField(11, 1))
  }

  it should "calculate 1 - 1 == 0 in GF(11)" in {
    val a = PrimeField(11, 1)

    (a - a) should be (PrimeField(11, 0))
  }

  it should "calculate 0 / 1 == 0 in GF(11)" in {
    val a = PrimeField(11, 0)
    val b = PrimeField(11, 1)

    (a / b) should be (PrimeField(11, 0))
  }

  it should "throw an arithmetic exception when 1 / 0 in GF(11)" in {
    an [java.lang.UnsupportedOperationException] should be thrownBy {
      val a = PrimeField(11, 1)
      val b = PrimeField(11, 0)

      (a / b)
    }
  }

  it should "calculate 4 / 4 == 1 in GF(11)" in {
    val a = PrimeField(11, 4)

    (a / a) should be (PrimeField(11, 1))
  }

  it should "calculate 3 - 4 == 10 in GF(11)" in {
    val a = PrimeField(11, 3)
    val b = PrimeField(11, 4)

    (a - b) should be (PrimeField(11, 10))
  }

  it should "calculate that (-3) == 8 in GF(11)" in {
    val a = PrimeField(11, 3)

    (-a) should be (PrimeField(11, 8))
  }

  it should "assert that 0 < 1 in GF(11)" in {
    val a = PrimeField(11, 0)
    val b = PrimeField(11, 1)

    (a < b) should be (true)
  }

  it should "assert that 1 > 0 in GF(11)" in {
    val a = PrimeField(11, 1)
    val b = PrimeField(11, 0)

    (a > b) should be (true)
  }
}
