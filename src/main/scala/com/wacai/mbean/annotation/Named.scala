package com.wacai.mbean.annotation

import java.util
import javax.management.ObjectName

import reflect._

object Named {
  def apply[T: ClassTag](name: String): ObjectName = apply(classTag[T].runtimeClass, name)

  def apply(clazz: Class[_], name: String): ObjectName = {
    val ht = new util.Hashtable[String, String]()
    ht.put("type", clazz.getSimpleName)
    ht.put("name", name)
    new ObjectName(clazz.getPackage.getName, ht)
  }
}

trait Named {
  def objectName: ObjectName
}