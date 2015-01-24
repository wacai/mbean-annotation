package com.wacai.mbean.actor

import javax.management._

import akka.actor.ActorRef
import akka.util.Timeout

import concurrent.Await
import concurrent.duration.FiniteDuration

object DynamicMBean {

  def apply(actor: ActorRef, atMost: FiniteDuration, info: MBeanInfo) = new DynamicMBean {
    import akka.pattern.ask

    implicit val timeout = Timeout(atMost)

    def getAttribute(name: String) = {
      Await.result(actor.ask(GetAttribute(name)).mapTo[AnyRef], atMost)
    }

    def getAttributes(names: Array[String]) = {
      Await.result(actor.ask(GetAttributes(names)).mapTo[AttributeList], atMost)
    }

    def setAttributes(attributes: AttributeList) = {
      Await.result(actor.ask(SetAttributes(attributes)).mapTo[AttributeList], atMost)
    }

    def setAttribute(attribute: Attribute) = {
      Await.result(actor.ask(SetAttribute(attribute)), atMost)
    }

    def invoke(actionName: String, params: Array[AnyRef], signature: Array[String]) = {
      Await.result(actor.ask(Invoke(actionName, params, signature)).mapTo[AnyRef], atMost)
    }

    def getMBeanInfo = info
  }

  case class SetAttributes(attrs: AttributeList)

  case class SetAttribute(attr: Attribute)

  case class GetAttributes(attrs: Array[String])

  case class GetAttribute(attr: String)

  case class Invoke(name: String, params: Array[AnyRef], signature: Array[String])

}
