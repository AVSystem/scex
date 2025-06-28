package com.avsystem.scex.util

import com.avsystem.commons.jiop.JavaInterop._
import com.google.common.collect.{Collections2, Lists, Sets}

/**
  * Java collection API extensions for SCEX expressions.
  */
object JavaCollectionExtensions {
  private type GFunction[F, T] = com.google.common.base.Function[F, T]
  private type JMapEntry[K, V] = java.util.Map.Entry[K, V]

  implicit final class CollectionOps[A](private val coll: JCollection[A]) extends AnyVal {
    def ++(other: JCollection[A]): JCollection[A] =
      (coll.asScala ++ other.asScala).asJavaCollection

    def filter(p: A => Boolean): JCollection[A] =
      coll.asScala.filter(p).asJavaCollection

    def map[B](f: A => B): JCollection[B] =
      coll.iterator.asScala.map(f).toBuffer.asJava

    def fold[B](z: B)(f: (B, A) => B): B =
      coll.asScala.foldLeft(z)(f)

    def containsAny(other: JCollection[_]): Boolean =
      other.exists(coll.contains)

    def find(p: A => Boolean): A =
      findOr(p, throw new NoSuchElementException("No element conforming to predicate found"))

    def findOr[B >: A](p: A => Boolean, default: => B): B =
      coll.asScala.find(p).getOrElse(default)

    def count(p: A => Boolean): Int =
      coll.asScala.count(p)

    def exists(p: A => Boolean): Boolean =
      coll.asScala.exists(p)

    def forall(p: A => Boolean): Boolean =
      coll.asScala.forall(p)

    def min(implicit ord: Ordering[A]): A =
      coll.asScala.min

    def minBy[B: Ordering](f: A => B): A =
      coll.asScala.minBy(f)

    def max(implicit ord: Ordering[A]): A =
      coll.asScala.max

    def maxBy[B: Ordering](f: A => B): A =
      coll.asScala.maxBy(f)

    def sum(implicit num: Numeric[A]): A =
      coll.asScala.sum

    def mean(implicit num: Numeric[A]): Double =
      if (!coll.isEmpty)
        num.toDouble(sum) / coll.size
      else
        throw new IllegalArgumentException("mean of empty collection")

    def toList: JList[A] = coll match {
      case list: JList[A] => list
      case _ => Lists.newArrayList[A](coll)
    }

    def toSet: JSet[A] = coll match {
      case set: JSet[A] => set
      case _ => Sets.newHashSet[A](coll)
    }

    def nonEmpty: Boolean =
      !coll.isEmpty

    def anyElement: A =
      anyElementOr(throw new NoSuchElementException("Collection is empty"))

    def anyElementOr[B >: A](default: => B): B =
      coll.asScala.headOption.getOrElse(default)
  }

  implicit final class StringCollectionOps(private val coll: JCollection[String]) extends AnyVal {
    def join(sep: String): String =
      coll.asScala.mkString(sep)

    def join: String =
      join("")
  }

  implicit final class ListOps[A](private val list: JList[A]) extends AnyVal {
    def ++(other: JList[A]): JList[A] =
      (list.asScala ++ other.asScala).asJava

    def filter(p: A => Boolean): JList[A] =
      list.asScala.filter(p).asJava

    def map[B](f: A => B): JList[B] =
      list.asScala.map(f).asJava

    def apply(index: Int): A =
      list.get(index)

    def getOr[B >: A](index: Int, default: => B): B =
      if (index >= 0 && index < list.size) list.get(index) else default

    def foldLeft[B](z: B)(f: (B, A) => B): B =
      list.asScala.foldLeft(z)(f)

    def foldRight[B](z: B)(f: (A, B) => B): B =
      list.asScala.foldRight(z)(f)

    def drop(amount: Int): JList[A] =
      slice(amount, list.size)

    def dropRight(amount: Int): JList[A] =
      slice(0, list.size - amount)

    def dropWhile(p: A => Boolean): JList[A] = {
      val firstIdx = list.asScala.indexWhere(a => !p(a))
      if (firstIdx >= 0) drop(firstIdx) else list
    }

    def take(amount: Int): JList[A] =
      slice(0, amount)

    def takeRight(amount: Int): JList[A] =
      slice(list.size - amount, list.size)

    def takeWhile(p: A => Boolean): JList[A] = {
      val firstIdx = list.asScala.indexWhere(a => !p(a))
      if (firstIdx >= 0) take(firstIdx) else list
    }

    def slice(from: Int, until: Int): JList[A] =
      list.subList(0 max from min list.size, 0 max until min list.size)

    def sorted(implicit ord: Ordering[A]): JList[A] =
      list.asScala.sorted.asJava

    def sortBy[B: Ordering](f: A => B): JList[A] =
      list.asScala.sortBy(f).asJava

    def reverse: JList[A] =
      Lists.reverse(list)
  }

  implicit final class SetOps[A](private val set: JSet[A]) extends AnyVal {
    def ++(other: JSet[A]): JSet[A] =
      (set.asScala ++ other.asScala).asJava

    def filter(p: A => Boolean): JSet[A] =
      set.asScala.filter(p).asJava

    def union(other: JSet[A]): JSet[A] =
      set ++ other

    def intersect(other: JSet[A]): JSet[A] =
      (set.asScala intersect other.asScala).asJava

    def diff(other: JSet[A]): JSet[A] =
      (set.asScala diff other.asScala).asJava
  }

  final case class Entry[K, V](key: K, value: V) {
    def withKey[NK](newKey: NK): Entry[NK, V] =
      copy(key = newKey)

    def withValue[NV](newValue: NV): Entry[K, NV] =
      copy(value = newValue)
  }

  trait MapOpsExtended[K, V] extends Any {
    def map: JMap[K, V]
    def update(key: K, value: V): Unit = map.put(key, value)
  }

  implicit final class MapOps[K, V](val map: JMap[K, V]) extends AnyVal with MapOpsExtended[K, V] {
    def ++(other: JMap[K, V]): JMap[K, V] =
      (map.asScala ++ other.asScala).asJava

    def apply(key: K): V =
      map.get(key)

    def getOr[U >: V](key: K, default: => U): U = {
      val res = map.get(key)
      if (res != null || map.containsKey(key)) res
      else default
    }

    def entries: JCollection[Entry[K, V]] =
      Collections2.transform(map.entrySet,
        new GFunction[JMapEntry[K, V], Entry[K, V]] {
          def apply(e: JMapEntry[K, V]): Entry[K, V] = Entry(e.getKey, e.getValue)
        })

    def filter(p: Entry[K, V] => Boolean): JMap[K, V] =
      map.asScala.iterator.filter(e => p(Entry(e._1, e._2))).toJMap

    def map[NK, NV](f: Entry[K, V] => Entry[NK, NV]): JMap[NK, NV] =
      map.asScala.iterator.map { e =>
        val ne = f(Entry(e._1, e._2))
        (ne.key, ne.value)
      }.toJMap

    def nonEmpty =
      !map.isEmpty
  }

  implicit final class EntryCollectionOps[K, V](private val entries: JCollection[Entry[K, V]]) extends AnyVal {
    def toMap: JMap[K, V] =
      entries.iterator.asScala.map(e => (e.key, e.value)).toJMap
  }

  implicit final class PairCollectionOps[K, V](private val entries: JCollection[(K, V)]) extends AnyVal {
    def toMap: JMap[K, V] =
      entries.iterator.asScala.toJMap
  }

  def list[A](elements: A*): JList[A] =
    elements.asJava

  def range(from: Int, to: Int, by: Int): JList[Int] =
    from.to(to).by(by).asJava

  def range(from: Int, to: Int): JList[Int] =
    range(from, to, 1)

  def set[A](elements: A*): JSet[A] =
    Sets.newHashSet[A](elements.asJava)

  def map[K, V](elements: (K, V)*): JMap[K, V] = {
    val result = new JHashMap[K, V]
    for ((k, v) <- elements) {
      result.put(k, v)
    }
    result
  }

  def entry[K, V](key: K, value: V): Entry[K, V] =
    Entry(key, value)
}
