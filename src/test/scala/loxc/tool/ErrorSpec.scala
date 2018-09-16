package loxc.tool

import org.scalatest.{FlatSpec, Matchers}

class ErrorSpec extends FlatSpec with Matchers {

  class MockClazz {
    def report: String = Error.format(1, this, "Testing error reporter.")
  }

  behavior of "Error Reporter"

  it should "provide formatted error report with canonical class name" in {
    val mock = new MockClazz
    mock.report should be("[Line 1] [Error] loxc.tool.ErrorSpec.MockClazz: Testing error reporter.")
  }
}
