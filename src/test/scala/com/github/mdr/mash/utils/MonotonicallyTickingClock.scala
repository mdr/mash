package com.github.mdr.mash.utils

import java.time.{ Clock, Instant, ZoneId }

object MonotonicallyTickingClock extends Clock {
  private val dummyClock = Clock.systemDefaultZone
  private var now = dummyClock.instant()

  override def getZone: ZoneId = dummyClock.getZone

  override def instant(): Instant = {
    now = now.plusSeconds(1)
    now
  }

  override def withZone(zone: ZoneId): Clock = dummyClock.withZone(zone)
}