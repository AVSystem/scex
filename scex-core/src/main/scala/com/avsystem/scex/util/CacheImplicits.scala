package com.avsystem.scex
package util

import java.{lang => jl, util => ju}

import com.google.common.cache.{CacheLoader, RemovalListener, RemovalNotification}

import scala.language.implicitConversions


object CacheImplicits {
  implicit def funToCacheLoader[K, V](fun: K => V) =
    new CacheLoader[K, V] {
      def load(key: K): V = fun(key)
    }

  implicit def funToRemovalListener[K, V](fun: RemovalNotification[K, V] => Unit) =
    new RemovalListener[K, V] {
      def onRemoval(notification: RemovalNotification[K, V]) {
        fun(notification)
      }
    }
}
