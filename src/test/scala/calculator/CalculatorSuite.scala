package calculator

import scala.collection.Set
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {
	
	/** ****************
	  * * TWEET LENGTH **
	  * *****************/
	
	def tweetLength(text: String): Int =
	text.codePointCount(0, text.length)
	
	test("tweetRemainingCharsCount with a constant signal") {
		val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
		assert(result() == MaxTweetLength - tweetLength("hello world"))
		
		val tooLong = "foo" * 200
		val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
		assert(result2() == MaxTweetLength - tweetLength(tooLong))
	}
	
	test("tweetRemainingCharsCount with a supplementary char") {
		val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
		assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
	}
	
	
	test("colorForRemainingCharsCount with a constant signal") {
		val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
		assert(resultGreen1() == "green")
		val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
		assert(resultGreen2() == "green")
		
		val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
		assert(resultOrange1() == "orange")
		val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
		assert(resultOrange2() == "orange")
		
		val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
		assert(resultRed1() == "red")
		val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
		assert(resultRed2() == "red")
	}
	
	test("Polynomial") {
		assert(!testPolynomial(0, 0, 0), "failed: 0, 0, 0")
		assert(testPolynomial(1, 2, 3), "failed: 1, 2, 3")
		assert(testPolynomial(1, 2, 1), "failed: 1, 2, 1")
		assert(testPolynomial(1, 3, 1), "failed: 1, 3, 1")
		assert(testPolynomial(2, 34, 5), "failed: 2, 34, 5")
		assert(testPolynomial(22, 101, 50), "failed: 22, 101, 50")
		assert(testPolynomial(2.2, 10.1, 5.0), "failed: 22, 101, 50")
	}
	
	def testPolynomial(a: Double, b: Double, c: Double): Boolean = {
		def aboutZero(v: Double): Boolean = math.abs(v) < 0.01
		//
		import Polynomial._
		val aSig = Signal(a)
		val bSig = Signal(b)
		val cSig = Signal(c)
		val D =  computeDelta(aSig, bSig, cSig)()
		val delta = computeDelta(aSig, bSig, cSig)
		val roots = computeSolutions(aSig, bSig, cSig, delta)()
//		println(s"($a, $b, $c);  delta= $D;  roots= $roots")
		roots.toList match {
			case Nil ⇒ {
//				println(s"D<0 ? ${D < 0}")
				D < 0
			}
			case v0 :: Nil ⇒ {
				val r0 = v0 * v0*a + v0*b + c
//				println(s"D==0 ? ${D==0};  r0= $r0; ")
				D == 0 && aboutZero(r0)
			}
			case v0 :: v1 :: Nil ⇒ {
				val r0 = v0 * v0*a + v0*b + c
				val r1 = v1 * v1*a + v1*b + c
//				println(s"D>0 ? ${D>0}  r0= $r0;  r1= $r1")
				D > 0 && aboutZero(r0) && aboutZero(r1)
			}
		}
	}
	
	test("Calc") {
		val v0 = Ref("1")
		val v1 = Ref("2")
		val v2 = Ref("3")
		val n0 = Literal(0)
		val n1 = Literal(1)
		val n2 = Literal(2)
		val nan = Double.NaN
		assert(testCalc(Expect(n1, 1), Expect(n1, 1), Expect(n1, 1)))
		assert(testCalc(Expect(n0, 0), Expect(n1, 1), Expect(n2, 2)))
		assert(testCalc(Expect(n1, 1), Expect(Plus(v0, n1), 2), Expect(n2, 2)))
		assert(testCalc(Expect(n1, 1), Expect(Plus(v1, n1), nan), Expect(n2, 2)))
		assert(testCalc(Expect(n1, 1), Expect(Plus(v1, n1), nan), Expect(v1, nan)))
	}
	
	def testCalc(v0: Expect, v1: Expect, v2: Expect): Boolean = {
		def equal(v0: Double, v1: Double) = (v0.isNaN && v1.isNaN ) || v0 == v1
		import Calculator._
		val map = Map(("1", Signal(v0.expr)), ("2", Signal(v1.expr)), ("3", Signal(v2.expr)))
		val results = computeValues(map)
		val e1 = equal(results("1")(), v0.res)
		val e2 = equal(results("2")(), v1.res)
		val e3 = equal(results("3")(), v2.res)
		println(List(e1, e2, e3))
//		println(s"TEST: 1: (${results("1")()} vs ${v0.res});  2: (${results("2")()} vs ${v1.res});  3: (${results("3")()} vs ${v2.res});  ")
		e1 && e2 && e3
	}
	
	case class Expect(expr: Expr, res: Double)
		
}
