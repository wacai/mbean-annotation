package com.wacai.mbean.annotation

import akka.actor.Actor

object ThrottleActor {

}

@mbean class ThrottleActor extends Actor {
  def receive = {
    case i: Int => count += i
  }

  var count = 0

  @attribute var threshold = 100
  @attribute var flag = false

  @operation def isOverload = count > threshold

}


