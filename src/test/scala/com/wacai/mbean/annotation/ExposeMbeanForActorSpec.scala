package com.wacai.mbean.annotation

import java.lang.management.ManagementFactory.{getPlatformMBeanServer => mServer}
import javax.management._

import akka.actor.{ActorSystem, Props}
import org.scalatest._

class ExposeMbeanForActorSpec extends FlatSpec with Matchers with BeforeAndAfterAll {


  import ActorTestEnv._

  "@mbean" should "expose mbean for an actor" in {
    val oname = Named[ThrottleActor](ref.path.name)
    ref ! 100
    Thread.sleep(100L) // wait for registion
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