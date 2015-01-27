package com.wacai.mbean.actor

import javax.management._
import management.ManagementFactory.{getPlatformMBeanServer => mServer}

import akka.actor._
import akka.util.Timeout
import com.wacai.mbean.annotation.Named

import concurrent.Await
import concurrent.duration.FiniteDuration
import util._

object AsMBean {

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

trait AsMBean extends Actor {
  it: Named with Ask =>

  import AsMBean._
  import collection.JavaConversions._

  @throws[Exception](classOf[Exception])
  override def postStop() = {
    try super.postStop() finally {
      mServer.unregisterMBean(_name)
    }
  }

  @throws[Exception](classOf[Exception])
  override def preStart() = {
    try super.preStart() finally {
      mServer.registerMBean(AsMBean(self, atMost, info), _name)
    }
  }

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

  val _attributes: Array[String]

  val _operations: Array[(String, Array[Class[_]])]

  private def info: MBeanInfo = {
    val attributeInfos = _attributes map { name =>
      val f = getClass.getDeclaredField(name)
      val b = f.getType == classOf[Boolean] || f.getType == classOf[java.lang.Boolean]
      new MBeanAttributeInfo(f.getName, f.getType.getName, "", true, true, b)
    }

    val operationInfos = _operations map {
      case (name, classes) => new MBeanOperationInfo("", getClass.getMethod(name, classes: _*))
    }

    new MBeanInfo(getClass.getName, "", attributeInfos, null, operationInfos, null)
  }

  private lazy val _name = objectName

  private def get(name: String): AnyRef = {
    val f = getClass.getDeclaredField(s"$name")
    f.setAccessible(true)
    f.get(this)
  }

  private def set(name: String, value: AnyRef): AnyRef = {
    val f = getClass.getDeclaredField(s"$name")
    f.setAccessible(true)
    f.set(this, value)
    value
  }

  private def invoke(name: String, params: Array[AnyRef], signature: Array[String]) = {
    getClass.getMethod(name, params.map(_.getClass): _*).invoke(this, params: _*)
  }
}
