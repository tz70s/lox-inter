package loxc.tool

import com.typesafe.scalalogging.Logger

trait LazyLog {
  lazy val log: Logger = Logger(this.getClass)
}
