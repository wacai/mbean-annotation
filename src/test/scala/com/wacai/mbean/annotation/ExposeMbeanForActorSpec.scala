package com.wacai.mbean.annotation

import java.lang.management.ManagementFactory.{getPlatformMBeanServer => mServer}
import javax.management._

import akka.actor.{ActorSystem, Props}
import org.scalatest._

import annotation.tailrec

class ExposeMbeanForActorSpec extends FlatSpec with Matchers with BeforeAndAfterAll {


  import ActorTestEnv._

  "@mbean" should "expose mbean for an actor" in {
    val oname = Named[ThrottleActor](ref.path.name)

    ref ! 100

    waitForRegistry(oname)

    mServer.invoke(oname, "isOverload", Array.empty[AnyRef], Array.empty[String]) shouldBe false
    mServer.setAttribute(oname, new Attribute("threshold", 99))
    mServer.invoke(oname, "isOverload", Array.empty[AnyRef], Array.empty[String]) shouldBe true
  }

  @tailrec
  private def waitForRegistry(oname: ObjectName) {
    try {
      Thread.sleep(100L)
      mServer.getObjectInstance(oname)
    } catch {
      case _: InstanceNotFoundException => waitForRegistry(oname)
      case t: Throwable                 => throw t
    }
  }

  val ref = system.actorOf(Props[ThrottleActor])

  override protected def afterAll() = {
    super.afterAll()
    system.shutdown()
  }
}

object ActorTestEnv {
  lazy val system = ActorSystem("test")

}