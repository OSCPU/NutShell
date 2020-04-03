package top

trait Common {
  val VAddrBits = 39
  val EnableDebug = false
}

trait BigCore {
  val EnableILA = true
  val HasL2cache = true
  val HasPrefetch = true
  val EnableMultiIssue = true
  val EnableSuperScalarExec = true
  val EnableOutOfOrderExec = true
  val HasITLB = true
  val HasDTLB = true
  val HasDcache = true
}

trait SmallCore {
  val EnableILA = true
  val HasL2cache = false
  val HasPrefetch = false
  val EnableMultiIssue = false
  val EnableSuperScalarExec = false
  val EnableOutOfOrderExec = false
  val HasITLB = false
  val HasDTLB = false
  val HasDcache = false
}

object Settings extends Common with SmallCore {}