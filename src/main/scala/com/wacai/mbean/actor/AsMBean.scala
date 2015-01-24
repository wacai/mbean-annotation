package com.wacai.mbean.actor

import javax.management._
import management.ManagementFactory

import akka.actor.Actor
import DynamicMBean._

import concurrent.duration.FiniteDuration

trait AsMBean extends Actor {

  import collection.JavaConversions._

  override def unhandled(message: Any) = message match {
    case SetAttributes(attrs) =>
      attrs.asList() foreach { a => set(a.getName, a.getValue)}
      sender() ! attrs

    case SetAttribute(attr) =>
      set(attr.getName, attr.getValue)
      sender() !()

    case GetAttributes(names) =>
      val attrs = names map { name => new Attribute(name, get(name))}
      sender() ! new AttributeList(attrs.toList)

    case GetAttribute(name) =>
      sender() ! get(name)

    case Invoke(name, params, signature) =>
      sender() ! invoke(name, params, signature)

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

  val _timeout: FiniteDuration

  def server = ManagementFactory.getPlatformMBeanServer

  def info: MBeanInfo

  def get(name: String): AnyRef

  def set(name: String, value: AnyRef): AnyRef

  def invoke(name: String, params: Array[AnyRef], signature: Array[String]): Any
}
