package com.wacai.mbean.annotation

import akka.actor.Actor

class ThrottleActor extends Actor with Throttle {
  override var threshold: Int = 100

  var count = 0

  def receive = {
    case i: Int => count += i
  }

  override def isOverload = count > threshold

  @throws[Exception](classOf[Exception])
  override def preStart() = super.preStart()

  @throws[Exception](classOf[Exception])
  override def postStop() = super.postStop()
}


