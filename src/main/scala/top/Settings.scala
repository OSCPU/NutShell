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
  val HasDTLB = true
  val HasITLB = true
  val HasDcache = true
  val HasIcache = true
}

trait SmallCore {
  val EnableILA = true
  val HasL2cache = false
  val HasPrefetch = false
  val EnableMultiIssue = false
  val EnableSuperScalarExec = false
  val EnableOutOfOrderExec = false
  val HasDTLB = false
  val HasITLB = false
  val HasDcache = false
  val HasIcache = false
}

object Settings extends Common with SmallCore {}