package top

trait BigCore {
  val EnableILA = true
  val HasL2cache = true
  val HasPrefetch = true
}

trait SmallCore {
  val EnableILA = true
  val HasL2cache = false
  val HasPrefetch = false
}

object Settings extends SmallCore {}