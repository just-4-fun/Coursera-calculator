package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
	private var vars = Set[String]()
	
	def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
		namedExpressions.map { case (vn, se) ⇒
			val exp = se()
			vars += vn
			val v = eval(exp, namedExpressions)
			if (!v.isNaN) vars -= vn
			(vn, Signal(v))
		}
	}
	
	def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
		case Literal(v) ⇒ v
		case Ref(v) ⇒
			if (vars.contains(v)) Double.NaN
			else {
				val n = eval(getReferenceExpr(v, references), references)
				if (n.isNaN) vars += v
				n
			}
		case Plus(v0, v1) ⇒ eval(v0, references) + eval(v1, references)
		case Minus(v0, v1) ⇒ eval(v0, references) - eval(v1, references)
		case Times(v0, v1) ⇒ eval(v0, references) * eval(v1, references)
		case Divide(v0, v1) ⇒
			try eval(v0, references) / eval(v1, references)
			catch {case _: Throwable ⇒ Double.NaN}
		case _ ⇒ Double.NaN
	}
	
	/** Get the Expr for a referenced variables.
	  * If the variable is not known, returns a literal NaN.
	  */
	private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]): Expr = {
		references.get(name).fold[Expr] {
			Literal(Double.NaN)
		} { exprSignal =>
			exprSignal()
		}
	}
}
