package com.wacai.mbean.annotation

import akka.actor.Actor

class ThrottleActor extends Actor with Throttle {
  var threshold: Int = 100

  var count = 0

  def receive = {
    case i: Int => count += i
  }

  def isOverload = count > threshold
}


