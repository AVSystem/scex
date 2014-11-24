package com.avsystem.scex.util

import java.{lang => jl, util => ju}

import com.google.common.collect.{Collections2, Lists, Sets}

import scala.collection.JavaConverters._

/**
 * Author: ghik
 * Created: 11/21/14.
 */
object JavaCollectionExtensions {

  import com.avsystem.scex.util.CommonUtils._

  implicit class CollectionOps[A](private val coll: ju.Collection[A]) extends AnyVal {
    def ++(other: ju.Collection[A]): ju.Collection[A] =
      (coll.asScala ++ other.asScala).asJavaCollection

    def filter(p: A => Boolean): ju.Collection[A] =
      coll.asScala.filter(p).asJavaCollection

    def map[B](f: A => B): ju.Collection[B] =
      coll.iterator.asScala.map(f).toBuffer.asJava

    def flatMap[B](f: A => ju.Collection[B]): ju.Collection[B] =
      coll.asScala.flatMap(a => f(a).asScala).asJavaCollection

    def find(p: A => Boolean): A =
      coll.asScala.find(p).get

    def count(p: A => Boolean): Int =
      coll.asScala.count(p)

    def exists(p: A => Boolean): Boolean =
      coll.asScala.exists(p)

    def forall(p: A => Boolean): Boolean =
      coll.asScala.forall(p)

    def foldLeft[B](z: B)(f: (B, A) => B): B =
      coll.asScala.foldLeft(z)(f)

    def foldRight[B](z: B)(f: (A, B) => B): B =
      coll.asScala.foldRight(z)(f)

    def min(implicit ord: Ordering[A]): A =
      coll.asScala.min

    def minBy[B: Ordering](f: A => B): A =
      coll.asScala.minBy(f)

    def max(implicit ord: Ordering[A]): A =
      coll.asScala.max

    def maxBy[B: Ordering](f: A => B): A =
      coll.asScala.maxBy(f)

    def toList: ju.List[A] = coll match {
      case list: ju.List[A] => list
      case _ => Lists.newArrayList[A](coll)
    }

    def toSet: ju.Set[A] = coll match {
      case set: ju.Set[A] => set
      case _ => Sets.newHashSet[A](coll)
    }

    def nonEmpty: Boolean =
      !coll.isEmpty

    def anyElement: A =
      coll.iterator.next
  }

  implicit class StringCollectionOps(private val coll: ju.Collection[String]) extends AnyVal {
    def join(sep: String): String =
      coll.asScala.mkString(sep)

    def join: String =
      join("")
  }

  implicit class ListOps[A](private val list: ju.List[A]) extends AnyVal {
    def ++(other: ju.List[A]): ju.List[A] =
      (list.asScala ++ other.asScala).asJava

    def filter(p: A => Boolean): ju.List[A] =
      list.asScala.filter(p).asJava

    def map[B](f: A => B): ju.List[B] =
      list.asScala.map(f).asJava

    def flatMap[B](f: A => ju.List[B]): ju.List[B] =
      list.asScala.flatMap(a => f(a).asScala).asJava

    def apply(index: Int): A =
      list.get(index)

    def drop(amount: Int): ju.List[A] =
      slice(amount, list.size)

    def dropRight(amount: Int): ju.List[A] =
      slice(0, list.size - amount)

    def dropWhile(p: A => Boolean): ju.List[A] = {
      val firstIdx = list.asScala.indexWhere(a => !p(a))
      if (firstIdx >= 0) drop(firstIdx) else list
    }

    def take(amount: Int): ju.List[A] =
      slice(0, amount)

    def takeRight(amount: Int): ju.List[A] =
      slice(list.size - amount, list.size)

    def takeWhile(p: A => Boolean): ju.List[A] = {
      val firstIdx = list.asScala.indexWhere(a => !p(a))
      if (firstIdx >= 0) take(firstIdx) else list
    }

    def slice(from: Int, until: Int): ju.List[A] =
      list.subList(0 max from min list.size, 0 max until min list.size)

    def sorted(implicit ord: Ordering[A]): ju.List[A] =
      list.asScala.sorted.asJava

    def sortBy[B: Ordering](f: A => B): ju.List[A] =
      list.asScala.sortBy(f).asJava
  }

  implicit class SetOps[A](private val set: ju.Set[A]) extends AnyVal {
    def ++(other: ju.Set[A]): ju.Set[A] =
      (set.asScala ++ other.asScala).asJava

    def filter(p: A => Boolean): ju.Set[A] =
      set.asScala.filter(p).asJava

    def union(other: ju.Set[A]): ju.Set[A] =
      set ++ other

    def intersect(other: ju.Set[A]): ju.Set[A] =
      (set.asScala intersect other.asScala).asJava

    def diff(other: ju.Set[A]): ju.Set[A] =
      (set.asScala diff other.asScala).asJava

    def containsAny(coll: ju.Collection[_]): Boolean =
      coll.asScala.exists(set.contains)
  }

  case class Entry[K, V](key: K, value: V) {
    def withKey[NK](newKey: NK): Entry[NK, V] =
      copy(key = newKey)

    def withValue[NV](newValue: NV): Entry[K, NV] =
      copy(value = newValue)
  }

  implicit class MapOps[K, V](private val map: ju.Map[K, V]) extends AnyVal {
    def ++(other: ju.Map[K, V]): ju.Map[K, V] =
      (map.asScala ++ other.asScala).asJava

    def apply(key: K): V =
      map.get(key)

    def entries: ju.Collection[Entry[K, V]] =
      Collections2.transform(map.entrySet, guavaFun((e: ju.Map.Entry[K, V]) => Entry(e.getKey, e.getValue)))

    def filter(p: Entry[K, V] => Boolean): ju.Map[K, V] =
      map.asScala.iterator.filter(e => p(Entry(e._1, e._2))).toMap.asJava

    def map[NK, NV](f: Entry[K, V] => Entry[NK, NV]): ju.Map[NK, NV] =
      map.asScala.iterator.map { e =>
        val ne = f(Entry(e._1, e._2))
        (ne.key, ne.value)
      }.toMap.asJava

    def nonEmpty =
      !map.isEmpty
  }

  implicit class EntryCollectionOps[K, V](private val entries: ju.Collection[Entry[K, V]]) extends AnyVal {
    def toMap: ju.Map[K, V] =
      entries.iterator.asScala.map(e => (e.key, e.value)).toMap.asJava
  }

  def list[A](elements: A*): ju.List[A] =
    elements.asJava

  def set[A](elements: A*): ju.Set[A] =
    Sets.newHashSet[A](elements.asJava)

  def map[K, V](elements: (K, V)*): ju.Map[K, V] = {
    val result = new ju.HashMap[K, V]
    for ((k, v) <- elements) {
      result.put(k, v)
    }
    result
  }

}
