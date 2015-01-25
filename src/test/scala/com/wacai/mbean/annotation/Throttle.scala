package com.wacai.mbean.annotation

import akka.actor.Actor

@mbean trait Throttle extends Actor {

   var threshold: Int = ???

   def isOverload: Boolean = ???

 }
