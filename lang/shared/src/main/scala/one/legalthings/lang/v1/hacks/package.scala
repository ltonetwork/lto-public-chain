package one.legalthings.lang

import one.legalthings.lang.v1.BaseGlobal

package object hacks {
  private[lang] val Global: BaseGlobal = one.legalthings.lang.Global // Hack for IDEA
}
