package com.wacai.mbean.annotation

import annotation.StaticAnnotation

class mbean extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Macro.impl
}
