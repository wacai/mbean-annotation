package com.wacai.mbean.actor

import concurrent.duration._

trait Ask {
  def atMost: FiniteDuration = 3 seconds
}
