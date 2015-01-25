package com.wacai.mbean.annotation


import management.ManagementFactory

import akka.actor.Actor

import reflect.macros._

class Macro(val c: whitebox.Context) {

  import c.universe._
  import Flag._

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val result = annottees.map(_.tree).toList match {
      case (ClassDef(mods @ IsTrait(), name, a, Template(parents @ WithActor(), s, body))) :: Nil =>

        val server = reify {ManagementFactory.getPlatformMBeanServer} tree

        val objectNameDef = if (parents.has[Named]) {
          q"lazy val _objectName = objectName"
        } else {
          q"lazy val _objectName = com.wacai.mbean.annotation.Named(getClass, self.path.name)"
        }

        val overridePreStartDef = {
          q"""
              @throws[Exception](classOf[Exception])
              override def preStart() = {
                super.preStart()
                println("???" + _objectName)
                $server.registerMBean(DynamicMBean(self, atMost, info), _objectName)
              }
           """
        }

        val overridePostStopDef = {
          q"""
              @throws[Exception](classOf[Exception])
              override def postStop() = {
                super.postStop()
                $server.unregisterMBean(_objectName)
              }
           """
        }

        val attributes = {
          val list = body collect { case ValDef(IsVar(), n, _, _) => q"${n.toString}"}
          q"Array(...$list)"
        }

        val operations = {
          val list = body collect {
            case DefDef(_, n, _, vparamss, _, _) =>
              val clss = vparamss.flatten collect { case ValDef(_, _, tpt, _) => q"classOf[$tpt]"}
              if (clss.isEmpty) q"(${n.toString}, Array.empty[Class[_]])" else q"($n, Array(...$clss))"
          }
          q"Array(...$list)"
        }

        val mbeanInfoDef =
          q"""
              def info = {
                import javax.management._
                val attributeInfos = $attributes map { name =>
                  val f = getClass.getField(name)
                  val b = f.getType == classOf[Boolean] || f.getType == classOf[java.lang.Boolean]
                  new MBeanAttributeInfo(f.getName, f.getType.getName, "", true, true, b)
                }

                val operationInfos = $operations map {
                  case (name, classes) => new MBeanOperationInfo("", getClass.getMethod(name, classes: _*))
                }

                new MBeanInfo(getClass.getName, "", attributeInfos, null, operationInfos, null)
              }
           """

        val nparents = parents.appendIfAbsent[AsMBean].appendIfAbsent[Ask]
        val nbody = objectNameDef :: overridePreStartDef :: overridePostStopDef :: mbeanInfoDef:: body
        ClassDef(mods, name, a, Template(nparents, s, nbody))

      case _ =>
        c.abort(c.enclosingPosition, "Annotation is only supported on trait with Actor")
    }

    println(result)

    c.Expr[Any](result)
  }

  def tpe(t: Tree): Type = c.typecheck(t).tpe

  def is[T: TypeTag](t: Type): Boolean = t <:< typeOf[T]

  def is[T: TypeTag](t: Tree): Boolean = is[T](tpe(q"0.asInstanceOf[$t]"))

  object IsTrait {
    def unapply(mods: Modifiers): Boolean = mods.hasFlag(DEFAULTPARAM | TRAIT)
  }

  object IsVar {
    def unapply(mods: Modifiers): Boolean = mods.hasFlag(MUTABLE)
  }

  object WithActor {
    def unapply(parents: List[Tree]): Boolean = parents.has[Actor]
  }

  implicit class SuperIn(parents: List[Tree]) {
    def has[T: TypeTag]: Boolean = parents exists is[T]

    def appendIfAbsent[T: TypeTag]: List[Tree] = if (has[T]) parents else q"${typeOf[T]}" :: parents
  }

}
