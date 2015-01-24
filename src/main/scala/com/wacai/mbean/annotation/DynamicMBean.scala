package com.wacai.mbean.annotation

import javax.management._

import akka.actor.ActorRef
import akka.util.Timeout

import concurrent.Await
import concurrent.duration.FiniteDuration
import util._

object DynamicMBean {

  def apply(actor: ActorRef, atMost: FiniteDuration, info: MBeanInfo) = new DynamicMBean {

    import akka.pattern.ask

    implicit val timeout = Timeout(atMost)


    def getAttribute(name: String) = {
      Await.result(actor.ask(GetAttribute(name)).mapTo[Try[AnyRef]], atMost) get
    }

    def getAttributes(names: Array[String]) = {
      Await.result(actor.ask(GetAttributes(names)).mapTo[Try[AttributeList]], atMost) get
    }

    def setAttributes(attributes: AttributeList) = {
      Await.result(actor.ask(SetAttributes(attributes)).mapTo[Try[AttributeList]], atMost) get
    }

    def setAttribute(attribute: Attribute) = {
      Await.result(actor.ask(SetAttribute(attribute)).mapTo[Try[Unit]], atMost) get
    }

    def invoke(actionName: String, params: Array[AnyRef], signature: Array[String]) = {
      Await.result(actor.ask(Invoke(actionName, params, signature)).mapTo[Try[AnyRef]], atMost) get
    }

    def getMBeanInfo = info
  }

  case class SetAttributes(attrs: AttributeList)

  case class SetAttribute(attr: Attribute)

  case class GetAttributes(attrs: Array[String])

  case class GetAttribute(attr: String)

  case class Invoke(name: String, params: Array[AnyRef], signature: Array[String])

}
