package me.stojan.reunion.structure.concrete

import me.stojan.reunion.ReunionSpec

class PrimeFieldSpec extends ReunionSpec {
  implicit val primeFieldDescriptor = PrimeField.descriptor(11)

  "PrimeField" should "calculate 1 + 10 == 0 in GF(11)" in {
    val a = PrimeField(1)
    val b = PrimeField(10)

    (a + b) should be (PrimeField(0))
  }

  it should "assign PF(12) to be PF(1)" in {
    PrimeField(12) should be (PrimeField(1))
  }

  it should "calculate 1 * 1 == 1 in GF(11)" in {
    val a = PrimeField(1)

    (a * a) should be (PrimeField(1))
  }

  it should "calculate 1 / 1 == 1 in GF(11)" in {
    val a = PrimeField(1)

    (a / a) should be (PrimeField(1))
  }

  it should "calculate 1 - 1 == 0 in GF(11)" in {
    val a = PrimeField(1)

    (a - a) should be (PrimeField(0))
  }

  it should "calculate 0 / 1 == 0 in GF(11)" in {
    val a = PrimeField(0)
    val b = PrimeField(1)

    (a / b) should be (PrimeField(0))
  }

  it should "throw an arithmetic exception when 1 / 0 in GF(11)" in {
    an [java.lang.UnsupportedOperationException] should be thrownBy {
      val a = PrimeField(1)
      val b = PrimeField(0)

      (a / b)
    }
  }

  it should "calculate 4 / 4 == 1 in GF(11)" in {
    val a = PrimeField(4)

    (a / a) should be (PrimeField(1))
  }

  it should "calculate 3 - 4 == 10 in GF(11)" in {
    val a = PrimeField(3)
    val b = PrimeField(4)

    (a - b) should be (PrimeField(10))
  }

  it should "calculate that (-3) == 8 in GF(11)" in {
    val a = PrimeField(3)

    (-a) should be (PrimeField(8))
  }

  it should "assert that 0 < 1 in GF(11)" in {
    val a = PrimeField(0)
    val b = PrimeField(1)

    (a < b) should be (true)
  }

  it should "assert that 1 > 0 in GF(11)" in {
    val a = PrimeField(1)
    val b = PrimeField(0)

    (a > b) should be (true)
  }

  it should "have a descriptor bound to the prime" in {
    val a = PrimeField(1)
    val b = PrimeField(3)
    val c = PrimeField(7)(PrimeField.descriptor(2))

    (a.descriptor) should be (b.descriptor)
    (b.descriptor) should be (a.descriptor)
    (c.descriptor) should not be (a.descriptor)
    (c.descriptor) should not be (b.descriptor)
  }

  it should "have a descriptor that returns the proper 0" in {
    val a = PrimeField(3)

    (a.descriptor.zero) should be (PrimeField(0))
  }

  it should "have a descriptor that returns the proper 1" in {
    val a = PrimeField(6)

    (a.descriptor.one) should be (PrimeField(1))
  }
}
