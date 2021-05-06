package com.ltonetwork.lang

import com.ltonetwork.lang.v1.BaseGlobal

package object hacks {
  private[lang] val Global: BaseGlobal = com.ltonetwork.lang.Global // Hack for IDEA
}
