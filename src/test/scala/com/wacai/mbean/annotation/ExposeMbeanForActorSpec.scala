package com.wacai.mbean.annotation

import javax.management.Attribute
import management.ManagementFactory.{getPlatformMBeanServer => mServer}

import akka.actor.{ActorSystem, Props}
import org.scalatest._

class ExposeMbeanForActorSpec extends FlatSpec with Matchers with BeforeAndAfterAll {

  import ActorTestEnv._

  "@mbean" should "expose mbean for an actor" in {
    val oname = Named[Throttle](ref.path.name)
    ref ! 100
    mServer.invoke(oname, "isOverload", Array.empty[AnyRef], Array.empty[String]) shouldBe false
    mServer.setAttribute(oname, new Attribute("threshold", 99))
    mServer.invoke(oname, "isOverload", Array.empty[AnyRef], Array.empty[String]) shouldBe true
  }

  lazy val ref = system.actorOf(Props[ThrottleActor])

  override protected def afterAll() = {
    super.afterAll()
    system.shutdown()
  }
}

object ActorTestEnv {
  lazy val system = ActorSystem("test")

}