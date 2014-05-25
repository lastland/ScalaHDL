import org.scalatest.Suite

import ScalaHDL.Core.DataType._
import ScalaHDL.Core.NotEnoughBitsException

object SignalTestHelper {
  def setAndTest(s: Signal, n: Int) {
    s.setNext(n)
    s.update()
  }
}

class UnsignedTest extends Suite {

  def testValid() {
    assert(new Unsigned("", 0, 1).value === 0)
    assert(new Unsigned("", 1, 1).value === 1)
    assert(new Unsigned("", 5, 3).value === 5)
    intercept[IllegalArgumentException] {
      new Unsigned("", -1, 1)
    }
    intercept[NotEnoughBitsException] {
      new Unsigned("", 0, 0)
    }
    intercept[NotEnoughBitsException] {
      new Unsigned("", 5, 1)
    }
    intercept[NotEnoughBitsException] {
      new Unsigned("", 32, 4)
    }
  }

  def testSetNext() {
    val a = new Unsigned("", 0, 4)
    assert(a.value === 0)
    SignalTestHelper.setAndTest(a, 1)
    assert(a.value === 1)
    SignalTestHelper.setAndTest(a, 15)
    assert(a.value === 15)
    intercept[NotEnoughBitsException] {
      SignalTestHelper.setAndTest(a, 16)
    }
    intercept[NotEnoughBitsException] {
      SignalTestHelper.setAndTest(a, 32)
    }
    intercept[IllegalArgumentException] {
      SignalTestHelper.setAndTest(a, -1)
    }
    intercept[IllegalArgumentException] {
      SignalTestHelper.setAndTest(a, -15)
    }
  }

  def testNegation() {
    assert(~new Unsigned("", 0, 1) === new Unsigned("", 1, 1))
    assert(~new Unsigned("", 0, 2) === new Unsigned("", 3, 2))
    assert(~new Unsigned("", 10, 4) === new Unsigned("", 5, 4))
    assert(~new Unsigned("", 31, 5) === new Unsigned("", 0, 5))
  }

  def testAdd() {
    assert(new Unsigned("", 0, 1) + new Unsigned("", 1, 1) ===
      new Unsigned("", 1, 1))
    assert(new Unsigned("", 15, 4) + new Unsigned("", 15, 4) ===
      new Unsigned("", 30, 5))
    assert(new Unsigned("", 1, 1) + new Unsigned("", 15, 4) ===
      new Unsigned("", 16, 5))
    assert(new Unsigned("", 0, 1) + new Signed("", -1, 2) ===
      new Signed("", -1, 2))
    assert(new Unsigned("", 3, 2) + new Signed("", -15, 5) ===
      new Signed("", -12, 5))
    assert(new Unsigned("", 15, 4) + new Signed("", -15, 5) ===
      new Signed("", 0, 1))
    assert(new Unsigned("", 15, 4) + new Signed("", -1, 2) ===
      new Signed("", 14, 5))
  }

  def testMinus() {
    assert(new Unsigned("", 0, 1) - new Unsigned("", 1, 1) ===
      new Signed("", -1, 2))
    assert(new Unsigned("", 15, 4) - new Unsigned("", 15, 4) ===
      new Signed("", 0, 1))
    assert(new Unsigned("", 1, 1) - new Unsigned("", 15, 4) ===
      new Signed("", -14, 5))
    assert(new Unsigned("", 0, 1) - new Signed("", -1, 2) ===
      new Signed("", 1, 2))
    assert(new Unsigned("", 3, 2) - new Signed("", -15, 5) ===
      new Signed("", 18, 6))
    assert(new Unsigned("", 15, 4) - new Signed("", -15, 5) ===
      new Signed("", 30, 6))
    assert(new Unsigned("", 15, 4) - new Signed("", -1, 2) ===
      new Signed("", 16, 6))
  }

  def testTimes() {
    assert(new Unsigned("", 1, 1) * new Unsigned("", 1, 1) ===
      new Unsigned("", 1, 1))
    assert(new Unsigned("", 15, 4) * new Unsigned("", 15, 4) ===
      new Unsigned("", 225, 8))
    assert(new Unsigned("", 1, 1) * new Unsigned("", 15, 4) ===
      new Unsigned("", 15, 4))
    assert(new Unsigned("", 1, 1) * new Signed("", -1, 2) ===
      new Signed("", -1, 2))
    assert(new Unsigned("", 3, 2) * new Signed("", -15, 5) ===
      new Signed("", -45, 7))
    assert(new Unsigned("", 15, 4) * new Signed("", -15, 5) ===
      new Signed("", -225, 9))
    assert(new Unsigned("", 15, 4) * new Signed("", -1, 2) ===
      new Signed("", -15, 5))
  }

  def testDivide() {
    assert(new Unsigned("", 1, 1) / new Unsigned("", 1, 1) ===
      new Unsigned("", 1, 1))
    assert(new Unsigned("", 15, 4) / new Unsigned("", 15, 4) ===
      new Unsigned("", 1, 1))
    assert(new Unsigned("", 1, 1) / new Unsigned("", 15, 4) ===
      new Unsigned("", 0, 1))
    assert(new Unsigned("", 1, 1) / new Signed("", -1, 2) ===
      new Signed("", -1, 2))
    assert(new Unsigned("", 3, 2) / new Signed("", -15, 5) ===
      new Unsigned("", 0, 1))
    assert(new Unsigned("", 15, 4) / new Signed("", -15, 5) ===
      new Signed("", -1, 2))
    assert(new Unsigned("", 15, 4) / new Signed("", -1, 2) ===
      new Signed("", -15, 5))
  }

  def testModulo() {
    assert(new Unsigned("", 1, 1) % new Unsigned("", 2, 2) ===
      new Unsigned("", 1, 1))
    assert(new Unsigned("", 15, 4) % new Unsigned("", 16, 5) ===
      new Unsigned("", 15, 4))
    assert(new Unsigned("", 15, 4) % new Unsigned("", 2, 2) ===
      new Unsigned("", 1, 1))
    assert(new Unsigned("", 10, 4) % new Unsigned("", 8, 4) ===
      new Unsigned("", 2, 2))
    assert(new Unsigned("", 10, 4) % new Unsigned("", 5, 3) ===
      new Unsigned("", 0, 1))
  }
}

class SignedTest extends Suite {

  def testValid() {
    assert(new Signed("", 0, 2).value === 0)
    assert(new Signed("", 1, 2).value === 1)
    assert(new Signed("", 5, 4).value === 5)
    assert(new Signed("", -5, 4).value === -5)
    assert(new Signed("", -1, 2).value === -1)
    intercept[NotEnoughBitsException] {
      new Signed("", -1, 1)
    }
    intercept[NotEnoughBitsException] {
      new Signed("", 0, 0)
    }
    intercept[NotEnoughBitsException] {
      new Signed("", 5, 1)
    }
    intercept[NotEnoughBitsException] {
      new Signed("", 32, 5)
    }
  }

  def testSetNext() {
    val a = new Signed("", 0, 5)
    assert(a.value === 0)
    SignalTestHelper.setAndTest(a, 1)
    assert(a.value === 1)
    SignalTestHelper.setAndTest(a, 15)
    assert(a.value === 15)
    SignalTestHelper.setAndTest(a, -1)
    assert(a.value === -1)
    SignalTestHelper.setAndTest(a, -16)
    assert(a.value === -16)
    intercept[NotEnoughBitsException] {
      SignalTestHelper.setAndTest(a, 16)
    }
    intercept[NotEnoughBitsException] {
      SignalTestHelper.setAndTest(a, 31)
    }
    intercept[NotEnoughBitsException] {
      SignalTestHelper.setAndTest(a, -17)
    }
    intercept[NotEnoughBitsException] {
      SignalTestHelper.setAndTest(a, -32)
    }
 }

  def testNegation() {
    assert(~new Signed("", 0, 2) === new Signed("", -1, 2))
    assert(~new Signed("", 1, 2) === new Signed("", -2, 2))
    assert(~new Signed("", 10, 5) === new Signed("", -11, 5))
    assert(~new Signed("", 31, 6) === new Signed("", -32, 6))
  }

  def testAdd() {
    assert(new Signed("", 0, 2) + new Signed("", 1, 2) ===
      new Signed("", 1, 2))
    assert(new Signed("", 15, 5) + new Signed("", 15, 5) ===
      new Signed("", 30, 6))
    assert(new Signed("", 1, 2) + new Signed("", 15, 5) ===
      new Signed("", 16, 6))
    assert(new Signed("", 0, 2) + new Signed("", -1, 2) ===
      new Signed("", -1, 2))
    assert(new Signed("", 3, 3) + new Signed("", -15, 5) ===
      new Signed("", -12, 5))
    assert(new Signed("", 15, 5) + new Signed("", -15, 5) ===
      new Signed("", 0, 1))
    assert(new Signed("", 15, 5) + new Signed("", -1, 2) ===
      new Signed("", 14, 5))
  }

  def testMinus() {
    assert(new Signed("", 0, 2) - new Signed("", 1, 3) ===
      new Signed("", -1, 2))
    assert(new Signed("", 15, 5) - new Signed("", 15, 5) ===
      new Signed("", 0, 1))
    assert(new Signed("", 1, 2) - new Signed("", 15, 5) ===
      new Signed("", -14, 5))
    assert(new Signed("", 0, 2) - new Signed("", -1, 2) ===
      new Signed("", 1, 2))
    assert(new Signed("", 3, 3) - new Signed("", -15, 5) ===
      new Signed("", 18, 6))
    assert(new Signed("", 15, 5) - new Signed("", -15, 5) ===
      new Signed("", 30, 6))
    assert(new Signed("", 15, 5) - new Signed("", -1, 2) ===
      new Signed("", 16, 6))
  }

  def testTimes() {
    assert(new Signed("", 1, 2) * new Signed("", 1, 2) ===
      new Signed("", 1, 2))
    assert(new Signed("", 15, 5) * new Signed("", 15, 5) ===
      new Signed("", 225, 9))
    assert(new Signed("", 1, 2) * new Signed("", 15, 5) ===
      new Signed("", 15, 5))
    assert(new Signed("", 1, 2) * new Signed("", -1, 2) ===
      new Signed("", -1, 2))
    assert(new Signed("", 3, 3) * new Signed("", -15, 5) ===
      new Signed("", -45, 7))
    assert(new Signed("", 15, 5) * new Signed("", -15, 5) ===
      new Signed("", -225, 9))
    assert(new Signed("", 15, 5) * new Signed("", -1, 2) ===
      new Signed("", -15, 5))
  }

  def testDivide() {
    assert(new Signed("", 1, 2) / new Signed("", 1, 2) ===
      new Signed("", 1, 2))
    assert(new Signed("", 15, 5) / new Signed("", 15, 5) ===
      new Signed("", 1, 2))
    assert(new Signed("", 1, 2) / new Signed("", 15, 5) ===
      new Signed("", 0, 1))
    assert(new Signed("", 1, 2) / new Signed("", -1, 2) ===
      new Signed("", -1, 2))
    assert(new Signed("", 3, 3) / new Signed("", -15, 5) ===
      new Signed("", 0, 1))
    assert(new Signed("", 15, 5) / new Signed("", -15, 5) ===
      new Signed("", -1, 2))
    assert(new Signed("", 15, 5) / new Signed("", -1, 2) ===
      new Signed("", -15, 5))
  }

  def testModulo() {
    assert(new Signed("", 1, 2) % new Unsigned("", 2, 2) ===
      new Unsigned("", 1, 1))
    assert(new Signed("", 15, 5) % new Unsigned("", 16, 5) ===
      new Unsigned("", 15, 4))
    assert(new Signed("", 15, 5) % new Unsigned("", 2, 2) ===
      new Unsigned("", 1, 1))
    assert(new Signed("", 10, 5) % new Unsigned("", 8, 4) ===
      new Unsigned("", 2, 2))
    assert(new Signed("", 10, 5) % new Unsigned("", 5, 3) ===
      new Unsigned("", 0, 1))
    assert(new Signed("", -1, 2) % new Unsigned("", 2, 2) ===
      new Unsigned("", 1, 1))
    assert(new Signed("", -1, 2) % new Unsigned("", 16, 5) ===
      new Unsigned("", 15, 4))
    assert(new Signed("", -15, 5) % new Unsigned("", 16, 5) ===
      new Unsigned("", 1, 1))
    assert(new Signed("", -15, 5) % new Unsigned("", 2, 2) ===
      new Unsigned("", 1, 1))
    assert(new Signed("", -10, 5) % new Unsigned("", 8, 4) ===
      new Unsigned("", 6, 3))
    assert(new Signed("", -10, 5) % new Unsigned("", 5, 3) ===
      new Unsigned("", 0, 1))
  }
}
