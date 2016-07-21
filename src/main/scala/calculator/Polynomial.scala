package calculator

object Polynomial {
	def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
		Signal(b() * b() - 4 * a() * c())
	}
	
	def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
		if (delta() < 0) {
//			println(s"COMP(${a()}, ${b()}, ${c()}, ${delta}):  None")
			Signal(Set())
		}
		else if (delta() == 0) {
			val v0 = -b() / (2 * a())
//			println(s"COMP(${a()}, ${b()}, ${c()}, ${delta}):  v0= $v0")
			Signal(Set(v0))
		}
		else {
			val v0 = (-b() + Math.sqrt(delta())) / (2 * a())
			val v1 = (-b() - Math.sqrt(delta())) / (2 * a())
//			println(s"COMP(${a()}, ${b()}, ${c()}, ${delta}):  v0= $v0;  v1= $v1")
			Signal(Set(v0, v1))
		}
	}
}
