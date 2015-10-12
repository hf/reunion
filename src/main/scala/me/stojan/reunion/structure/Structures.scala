package me.stojan.reunion.structure

/**
 * Generic algebraic element.
 */
trait Element[V] {
  def value: V
}

/**
 * Describes the structure to which an element belongs.
 */
trait Descriptor[V, E <: Element[V]] {
  /**
   * Obtain a structured element from the internal type.
   */
  def obtain(value: V): E
}

/**
 * Describes a group.
 */
trait GroupDescriptor[V] extends Descriptor[V, Group[V]] {
  /**
   * Identity element of the group.
   */
  def identity: Group[V]
}

/**
 * Describes a ring.
 */
trait RingDescriptor[V] extends Descriptor[V, Ring[V]] {
  /**
   * Additive identity element.
   */
  def zero: Ring[V]

  /**
   * Multiplicative identity element.
   */
  def one: Ring[V]
}

/**
 * Describes a field.
 */
trait FieldDescriptor[V] extends Descriptor[V, Field[V]] {
  /**
   * Additive identity element.
   */
  def zero: Field[V]

  /**
   * Multiplicative identity element.
   */
  def one: Field[V]
}

/**
 * A generic Abelian Group element.
 */
trait Group[V] extends Element[V] {
  /**
   * This group's descriptor.
   */
  def descriptor: GroupDescriptor[V]

  /**
   * Whether this is the identity element.
   */
  def isIdentity: Boolean

  def nonIdentity: Boolean = !isIdentity

  /**
   * Check whether this group element is greater than (partially ordered above)
   * the group element `g`.
   */
  def >(g: Group[V]): Boolean

  /**
   * Check whether this group element is greater than or equal
   * (partially ordered above) to the group element `g`.
   */
  def >=(g: Group[V]): Boolean = (this > g) || (this == g)

  /**
   * Check whether this group element is less than (partially ordered below) the
   * group element `g`.
   */
  def <(g: Group[V]): Boolean

  /**
   * Check whether this group element is less than or equal (partially ordered
   * below) the group element `g`.
   */
  def <=(g: Group[V]): Boolean = (this < g) || (this == g)

  /**
   * Inverse element. (Additive inverse.)
   */
  def unary_-(): Group[V]

  /**
   * Commutative operation of the group. (Addition.)
   */
  def +(g: Group[V]): Group[V]

  /**
   * Non-commutative inverse addition.
   */
  def -(g: Group[V]): Group[V] = this + (-g)
}

/**
 * A generic Ring element.
 */
trait Ring[V] extends Element[V] {
  /**
   * This ring's descriptor.
   */
  def descriptor: RingDescriptor[V]

  /**
   * Whether this is the additive identity element.
   */
  def isZero: Boolean
  def nonZero: Boolean = !isZero

  /**
   * Whether this is the multiplicative identity element.
   */
  def isOne: Boolean
  def nonOne: Boolean = !isOne

  /**
   * Check whether this group element is greater than (partially ordered above)
   * the group element `r`.
   */
  def >(r: Ring[V]): Boolean

  /**
   * Check whether this ring element is greater than or equal
   * (partially ordered above) to the ring element `r`.
   */
  def >=(r: Ring[V]): Boolean = (this > r) || (this == r)

  /**
   * Check whether this ring element is less than (partially ordered below) the
   * ring element `r`.
   */
  def <(r: Ring[V]): Boolean

  /**
   * Check whether this ring element is less than or equal (partially ordered
   * below) the ring element `r`.
   */
  def <=(r: Ring[V]): Boolean = (this < r) || (this == r)

  /**
   * Additive inverse element.
   */
  def unary_-(): Ring[V]

  /**
   * Commutative operation on all elements in the set. (Addition.)
   */
  def +(r: Ring[V]): Ring[V]

  /**
   * Non-commutative operation on all elements in the set. (Inverse addition.)
   */
  def -(r: Ring[V]): Ring[V] = this + (-r)

  /**
   * Associative multiplication operation on all elements in the set. (Need
   * not be commutative.) It also must be distributive over addition.
   */
  def *(r: Ring[V]): Ring[V]
}

/**
 * A generic field element.
 *
 * Elements are assumed to have partial ordering.
 */
trait Field[V] extends Element[V] {
  /**
   * This field's descriptor.
   */
  def descriptor: FieldDescriptor[V]

  /**
   * Whether this is the additive identity element.
   */
  def isZero: Boolean
  def nonZero: Boolean = !isZero

  /**
   * Whether this is the multiplicative identity element.
   */
  def isOne: Boolean
  def nonOne: Boolean = !isOne

  /**
   * Check whether this field element is greater than (partially ordered above)
   * the field element `f`.
   */
  def >(f: Field[V]): Boolean

  /**
   * Check whether this field element is greater than or equal
   * (partially ordered above) to the field element `f`.
   */
  def >=(f: Field[V]): Boolean = (this > f) || (this == f)

  /**
   * Check whether this field element is less than (partially ordered below) the
   * field element `f`.
   */
  def <(f: Field[V]): Boolean

  /**
   * Check whether this field element is less than or equal (partially ordered
   * below) the field element `f`.
   */
  def <=(f: Field[V]): Boolean = (this < f) || (this == f)

  /**
   * Additive inverse.
   */
  def unary_-(): Field[V]

  /**
   * Multiplicative inverse. (Not defined for `(this - this)`.)
   */
  def unary_~(): Field[V]

  /**
   * Commutative addition.
   */
  def +(f: Field[V]): Field[V]

  /**
   * Non-commutative inverse addition.
   */
  def -(f: Field[V]): Field[V] = this + (-f)

  /**
   * Commutative multiplication.
   */
  def *(f: Field[V]): Field[V]

  /**
   * Non-commutative inverse multiplication. (Not defined if `f == f - f`).
   */
  def /(f: Field[V]): Field[V] = this * (~f)
}

object Group {
  private case class RAdditiveGroupDescriptor[V](ringDescriptor: RingDescriptor[V]) extends GroupDescriptor[Ring[V]] {
    override lazy val identity: Group[Ring[V]] = RAdditiveGroup(ringDescriptor.zero)
    override def obtain(value: Ring[V]): Group[Ring[V]] = RAdditiveGroup(value)
  }

  private case class RAdditiveGroup[V](value: Ring[V]) extends Group[Ring[V]] {
    override val descriptor: GroupDescriptor[Ring[V]] = RAdditiveGroupDescriptor(value.descriptor)

    override def isIdentity: Boolean = value.isZero
    override def +(g: Group[Ring[V]]): Group[Ring[V]] = value + g.value
    override def unary_-(): Group[Ring[V]] = -value
    override def -(g: Group[Ring[V]]): Group[Ring[V]] = value - g.value

    override def >(g: Group[Ring[V]]): Boolean = value > g.value
    override def <(g: Group[Ring[V]]): Boolean = value < g.value
    override def <=(g: Group[Ring[V]]): Boolean = value <= g.value
    override def >=(g: Group[Ring[V]]): Boolean = value >= g.value

    private implicit def ringToGroup(r: Ring[V]): Group[Ring[V]] = RAdditiveGroup(r)
  }

  private case class FAdditiveGroupDescriptor[V](fieldDescriptor: FieldDescriptor[V]) extends GroupDescriptor[Field[V]] {
    override lazy val identity: Group[Field[V]] = obtain(fieldDescriptor.zero)
    override def obtain(value: Field[V]): Group[Field[V]] = FAdditiveGroup(value)
  }

  private case class FAdditiveGroup[V](value: Field[V]) extends Group[Field[V]] {
    override val descriptor: GroupDescriptor[Field[V]] = FAdditiveGroupDescriptor(value.descriptor)

    override def isIdentity: Boolean = value.isZero
    override def +(f: Group[Field[V]]): Group[Field[V]] = value + f.value
    override def unary_-(): Group[Field[V]] = -value
    override def -(f: Group[Field[V]]): Group[Field[V]] = value - f.value

    override def >(f: Group[Field[V]]): Boolean = value > f.value
    override def <(f: Group[Field[V]]): Boolean = value < f.value
    override def <=(f: Group[Field[V]]): Boolean = value <= f.value
    override def >=(f: Group[Field[V]]): Boolean = value >= f.value

    private implicit def fieldToGroup(f: Field[V]): Group[Field[V]] = FAdditiveGroup(f)
  }

  def additive[V](ring: Ring[V]): Group[Ring[V]] = RAdditiveGroup(ring)
  def additive[V](field: Field[V]): Group[Field[V]] = FAdditiveGroup(field)
}

object Ring {
  private case class FRingDescriptor[V](fieldDescriptor: FieldDescriptor[V]) extends RingDescriptor[Field[V]] {
    override lazy val zero: Ring[Field[V]] = obtain(fieldDescriptor.zero)
    override lazy val one: Ring[Field[V]] = obtain(fieldDescriptor.one)

    override def obtain(value: Field[V]): Ring[Field[V]] = FRing(value)
  }

  private case class FRing[V](value: Field[V]) extends Ring[Field[V]] {
    override val descriptor: RingDescriptor[Field[V]] = FRingDescriptor(value.descriptor)

    override def isZero: Boolean = value.isZero
    override def isOne: Boolean = value.isOne
    override def +(r: Ring[Field[V]]): Ring[Field[V]] = value + r.value
    override def -(r: Ring[Field[V]]): Ring[Field[V]] = value - r.value
    override def unary_-(): Ring[Field[V]] = -value
    override def *(r: Ring[Field[V]]): Ring[Field[V]] = value * r.value

    override def >(r: Ring[Field[V]]): Boolean = value > r.value
    override def <(r: Ring[Field[V]]): Boolean = value < r.value
    override def <=(r: Ring[Field[V]]): Boolean = value <= r.value
    override def >=(r: Ring[Field[V]]): Boolean = value >= r.value

    private implicit def fieldToRing(f: Field[V]): Ring[Field[V]] = FRing(f)
  }

  def apply[V](field: Field[V]): Ring[Field[V]] = FRing(field)
}

/**
 * Holds implicits for converting `Field`s into `Ring`s and `Group`s.
 */
object FieldConversions {
  /**
   * Exposes the field's additive abelian group.
   */
  implicit def fieldToGroup[V](f: Field[V]): Group[Field[V]] = Group.additive(f)

  implicit def fieldToRing[V](f: Field[V]): Ring[Field[V]] = Ring.apply(f)
}

/**
 * Holds implicits for converting `Ring`s to `Group`s.
 */
object RingConversions {
  /**
   * Exposes the ring's additive abelian group.
   */
  implicit def ringToGroup[V](r: Ring[V]): Group[Ring[V]] = Group.additive(r)
}
