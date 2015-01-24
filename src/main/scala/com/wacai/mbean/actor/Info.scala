package com.wacai.mbean.actor

import java.util
import javax.management.ObjectName

import akka.actor.Actor

trait Info extends Actor {
  def objectName: ObjectName = {
    val ht = new util.Hashtable[String, String]()
    ht.put("type", getClass.getSimpleName)
    ht.put("name", self.path.name)
    new ObjectName(getClass.getPackage.getName, ht)
  }

  def description: String = ""
}
