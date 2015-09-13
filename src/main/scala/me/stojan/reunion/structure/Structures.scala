package me.stojan.reunion.structure

/**
 * Generic algebraic element.
 */
trait Element[V] {
  def value: V
}

/**
 * A generic Abelian Group element.
 */
trait Group[V] extends Element[V] {
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
 */
trait Field[V] extends Element[V] {
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

/**
 * Holds implicits for converting `Field`s into `Ring`s and `Group`s.
 */
object FieldConversions {
  private case class RingField[V](field: Field[V]) extends Ring[V] {
    override def value: V = field.value
    override def unary_-(): Ring[V] = -field
    override def +(r: Ring[V]): Ring[V] = field + r
    override def -(r: Ring[V]): Ring[V] = field - r
    override def *(r: Ring[V]): Ring[V] = field * r
  }

  private case class GroupField[V](field: Field[V]) extends Group[V] {
    override def value: V = field.value
    override def unary_-(): Group[V] = -field
    override def +(g: Group[V]): Group[V] = field + g
    override def -(g: Group[V]): Group[V] = field - g
  }

  implicit def fieldToRing[V](f: Field[V]): Ring[V] = RingField[V](f)

  /**
   * Exposes the field's additive abelian group.
   */
  implicit def fieldToGroup[V](f: Field[V]): Group[V] = GroupField[V](f)
}

/**
 * Holds implicits for converting `Ring`s to `Group`s.
 */
object RingConversions {
  private case class GroupRing[V](ring: Ring[V]) extends Group[V] {
    override def value: V = ring.value
    override def unary_-(): Group[V] = -ring
    override def +(g: Group[V]): Group[V] = ring + g
    override def -(g: Group[V]): Group[V] = ring - g
  }

  implicit def ringToGroup[V](r: Ring[V]): Group[V] = GroupRing[V](r)
}
