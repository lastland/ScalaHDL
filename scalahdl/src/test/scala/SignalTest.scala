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
    SignalTestHelper.setAndTest(a, 16)
    assert(a.value === 0)
    SignalTestHelper.setAndTest(a, 31)
    assert(a.value === 15)
    SignalTestHelper.setAndTest(a, -1)
    assert(a.value === 15)
    SignalTestHelper.setAndTest(a, -15)
    assert(a.value === 1)
  }

  def testOpposite() {
    assert(new Unsigned("", 0, 1).opposite === new Unsigned("", 1, 1))
    assert(new Unsigned("", 0, 2).opposite === new Unsigned("", 3, 2))
    assert(new Unsigned("", 10, 4).opposite === new Unsigned("", 5, 4))
    assert(new Unsigned("", 31, 5).opposite === new Unsigned("", 0, 5))
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
      new Unsigned("", 0, 1))
    assert(new Unsigned("", 15, 4) + new Signed("", -1, 2) ===
      new Unsigned("", 14, 4))
  }

  def testMinus() {
    assert(new Unsigned("", 0, 1) - new Unsigned("", 1, 1) ===
      new Signed("", -1, 2))
    assert(new Unsigned("", 15, 4) - new Unsigned("", 15, 4) ===
      new Unsigned("", 0, 1))
    assert(new Unsigned("", 1, 1) - new Unsigned("", 15, 4) ===
      new Signed("", -14, 5))
    assert(new Unsigned("", 0, 1) - new Signed("", -1, 2) ===
      new Unsigned("", 1, 1))
    assert(new Unsigned("", 3, 2) - new Signed("", -15, 5) ===
      new Unsigned("", 18, 5))
    assert(new Unsigned("", 15, 4) - new Signed("", -15, 5) ===
      new Unsigned("", 30, 5))
    assert(new Unsigned("", 15, 4) - new Signed("", -1, 2) ===
      new Unsigned("", 16, 5))
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
}