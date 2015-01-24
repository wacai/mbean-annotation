package com.wacai.mbean.actor

import java.lang.Boolean
import java.lang.reflect.Field
import javax.management._
import management.ManagementFactory

import akka.actor.Actor
import DynamicMBean._

import concurrent.duration.FiniteDuration
import util.Try

trait AsMBean extends Actor {

  import collection.JavaConversions._

  override def unhandled(message: Any) = message match {
    case SetAttributes(attrs) =>
      sender() ! Try(attrs.asList() foreach { a => set(a.getName, a.getValue)}).map { _ => attrs}

    case SetAttribute(attr) =>
      sender() ! Try(set(attr.getName, attr.getValue))

    case GetAttributes(names) =>
      sender() ! Try(names map { n => new Attribute(n, get(n))}).map { as => new AttributeList(as.toList)}

    case GetAttribute(name) =>
      sender() ! Try(get(name))

    case Invoke(name, params, signature) =>
      sender() ! Try(invoke(name, params, signature))

    case _ => super.unhandled(message)
  }

  @throws[Exception](classOf[Exception])
  override def preStart() = {
    server.registerMBean(DynamicMBean(self, _timeout, info), _objectName)
    super.preStart()
  }

  @throws[Exception](classOf[Exception])
  override def postStop() = {
    server.unregisterMBean(_objectName)
    super.postStop()
  }

  val _objectName: ObjectName
  val _timeout   : FiniteDuration
  val _attributes: Array[String]
  val _operations: Array[(String, Array[Class[_]])]

  private def server = ManagementFactory.getPlatformMBeanServer

  private def info: MBeanInfo = {
    val attributeInfos = _attributes map { name =>
      val f = getClass.getField(name)
      val b = f.getType == classOf[Boolean] || f.getType == classOf[java.lang.Boolean]
      new MBeanAttributeInfo(f.getName, f.getType.getName, "", true, true, b)
    }

    val operationInfos = _operations map {
      case (name, classes) => new MBeanOperationInfo("", getClass.getMethod(name, classes: _*))
    }

    new MBeanInfo(getClass.getName, "", attributeInfos, null, operationInfos, null)
  }

  private def get(name: String): AnyRef = getClass.getMethod(s"$name").invoke(this)

  private def set(name: String, value: AnyRef): AnyRef = {
    getClass.getMethod(s"${name}_=", value.getClass).invoke(this, value)
    value
  }

  private def invoke(name: String, params: Array[AnyRef], signature: Array[String]) = {
    getClass.getMethod(name, params.map(_.getClass): _*).invoke(this, params)
  }
}
