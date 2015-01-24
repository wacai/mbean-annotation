package com.wacai.mbean.annotation

import reflect.macros._

class Macro(val c: whitebox.Context) {
  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    ???
  }
}
