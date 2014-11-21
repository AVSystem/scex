package com.avsystem.scex.util

import java.{lang => jl, util => ju}

import com.google.common.collect.{Lists, Sets}

import scala.collection.JavaConverters._

/**
 * Author: ghik
 * Created: 11/21/14.
 */
object JavaCollectionExtensions {

  implicit class CollectionOps[A](private val coll: ju.Collection[A]) extends AnyVal {
    def ++(other: ju.Collection[A]): ju.Collection[A] =
      (coll.asScala ++ other.asScala).asJavaCollection

    def filter(p: A => Boolean): ju.Collection[A] =
      coll.asScala.filter(p).asJavaCollection

    def map[B](f: A => B): ju.Collection[B] =
      coll.asScala.map(f).asJavaCollection

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

  def list[A](elements: A*): ju.List[A] =
    elements.asJava

  def set[A](elements: A*): ju.Set[A] =
    Sets.newHashSet[A](elements.asJava)

}
