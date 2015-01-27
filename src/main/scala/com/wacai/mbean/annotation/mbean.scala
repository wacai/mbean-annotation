package com.wacai.mbean.annotation

import akka.actor.Actor
import com.wacai.mbean.actor._

import reflect.macros.whitebox
import scala.annotation.StaticAnnotation

class mbean(description: String = "No description") extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Macro.impl
}

class Macro(val c: whitebox.Context) {

  import c.universe._
  import Flag._

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {

    val newClassDef: ClassDef => ClassDef = {
      case (ClassDef(mods, name, tp, Template(parents @ WithActor(), s, body))) =>

        val attributesDef = {
          val list = body collect { case ValDef(IsAttribute(), n, _, _) => q"${n.toString}"}
          q"val _attributes = Array(..$list)"
        }

        val operationsDef = {
          val list = body collect {
            case DefDef(IsOperation(), n, _, vparamss, _, _) =>
              val clss = vparamss.flatten collect { case ValDef(_, _, tpt, _) => q"classOf[$tpt]"}
              if (clss.isEmpty) q"(${n.toString}, Array.empty[Class[_]])" else q"($n, Array(...$clss))"
          }
          q"val _operations: Array[(String, Array[Class[_]])] = Array(..$list)"
        }

        val nparents = {
          val p = parents.appendIfAbsent[AsMBean].appendIfAbsent[Ask]
          if (p.has[Named]) p else q"${typeOf[DefaultNamed]}" :: p
        }

        val nbody = attributesDef :: operationsDef :: body

        ClassDef(mods, name, tp, Template(nparents, s, nbody))

    }

    val result = annottees.map(_.tree).toList match {
      case (c: ClassDef) :: Nil =>
        newClassDef(c)

      case (c: ClassDef) :: o :: Nil =>
        q"..${newClassDef(c) :: o :: Nil}"

      case _ =>
        c.abort(c.enclosingPosition, "Annotation is only supported on class with Actor")
    }

    c.Expr[Any](result)
  }

  def tpe(t: Tree): Type = c.typecheck(t).tpe

  def is[T: TypeTag](t: Type): Boolean = t <:< typeOf[T]

  def is[T: TypeTag](t: Tree): Boolean = is[T](tpe(q"0.asInstanceOf[$t]"))

  object IsTrait {
    def unapply(mods: Modifiers): Boolean = mods.hasFlag(DEFAULTPARAM | TRAIT)
  }

  object IsAttribute {
    def unapply(mods: Modifiers): Boolean = mods.hasFlag(MUTABLE) && mods.annotations.has[attribute]
  }

  object IsOperation {
    def unapply(mods: Modifiers): Boolean = mods.annotations.has[operation]
  }

  object WithActor {
    def unapply(parents: List[Tree]): Boolean = parents.has[Actor]
  }

  implicit class SuperIn(parents: List[Tree]) {
    def has[T: TypeTag]: Boolean = parents exists is[T]

    def appendIfAbsent[T: TypeTag]: List[Tree] = if (has[T]) parents else q"${typeOf[T]}" :: parents
  }

}
