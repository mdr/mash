package com.github.mdr.mash.utils

import java.time.{ Instant, LocalDate, LocalDateTime, ZoneId }

object TimeUtils {

  def localDateTime(instant: Instant): LocalDateTime = LocalDateTime.ofInstant(instant, ZoneId.systemDefault)

  def localDate(instant: Instant): LocalDate = localDateTime(instant).toLocalDate

}
