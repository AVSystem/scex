package com.avsystem.scex.util

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

/**
  * Author: ghik
  * Created: 25/01/16.
  */
class NamingThreadFactory(prefix: String) extends ThreadFactory {
  private val group: ThreadGroup = {
    val s: SecurityManager = System.getSecurityManager
    if (s != null) s.getThreadGroup else Thread.currentThread.getThreadGroup
  }

  private val threadNo: AtomicInteger = new AtomicInteger(1)

  def newThread(r: Runnable) = {
    val t = new Thread(group, r, prefix + "-" + threadNo.getAndIncrement(), 0)
    if (t.isDaemon) t.setDaemon(false)
    if (t.getPriority != Thread.NORM_PRIORITY) t.setPriority(Thread.NORM_PRIORITY)
    t
  }
}
