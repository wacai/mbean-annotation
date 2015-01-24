package com.wacai.mbean.annotation

import concurrent.duration._

trait Ask {
  def atMost: FiniteDuration = 3 seconds
}
