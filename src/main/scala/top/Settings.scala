package top

trait Common {
  val EnableILA = true
  val VAddrBits = 39
  val EnableDebug = false
}

trait OoOCore {
  val HasL2cache = true
  val HasPrefetch = true
  val EnableMultiIssue = true
  val EnableSuperScalarExec = true
  val EnableOutOfOrderExec = true
  val HasDTLB = true
  val HasITLB = true
  val HasDcache = true
  val HasIcache = true
  val MmodeOnly = false
}

trait SeqCore {
  val HasL2cache = false
  val HasPrefetch = false
  val EnableMultiIssue = false
  val EnableSuperScalarExec = false
  val EnableOutOfOrderExec = false
  val HasDTLB = true
  val HasITLB = true
  val HasDcache = true
  val HasIcache = true
  val MmodeOnly = false
}

trait SmallCore {
  val HasL2cache = false
  val HasPrefetch = false
  val EnableMultiIssue = false
  val EnableSuperScalarExec = false
  val EnableOutOfOrderExec = false
  val HasDTLB = false
  val HasITLB = false
  val HasDcache = false
  val HasIcache = false
  val MmodeOnly = true
}

object Settings extends Common with SmallCore {}