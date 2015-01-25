package com.wacai.mbean.actor

import akka.actor.Actor
import com.wacai.mbean.annotation.Named

trait DefaultNamed extends Actor with Named {
  def objectName = Named(getClass, self.path.name)
}
