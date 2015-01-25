package com.wacai.mbean.annotation

import javax.management._

import DynamicMBean._
import akka.actor.Actor

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


  private def get(name: String): AnyRef = getClass.getMethod(s"$name").invoke(this)

  private def set(name: String, value: AnyRef): AnyRef = {
    getClass.getMethod(s"${name}_=", value.getClass).invoke(this, value)
    value
  }

  private def invoke(name: String, params: Array[AnyRef], signature: Array[String]) = {
    getClass.getMethod(name, params.map(_.getClass): _*).invoke(this, params)
  }
}