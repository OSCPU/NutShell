package top

trait Common {
  var VAddrBits = 39
}

trait BigCore {
  val EnableILA = true
  val HasL2cache = true
  val HasPrefetch = true
  val EnableMultiIssue = true
  val EnableSuperScalarExec = true
  val EnableOutOfOrderExec = true
}

trait SmallCore {
  val EnableILA = true
  val HasL2cache = false
  val HasPrefetch = false
  val EnableMultiIssue = false
  val EnableSuperScalarExec = false
  val EnableOutOfOrderExec = false
}

object Settings extends Common with SmallCore {}