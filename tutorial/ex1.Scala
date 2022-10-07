/* Abstract syntax tree */
abstract class Expr
// Arithmetic
case class Num(n: Integer) extends Expr
case class Plus(e1: Expr, e2: Expr) extends Expr
case class Times(e1: Expr, e2: Expr) extends Expr

/* Traverses a list and returns all elements 
in even-numbered positions. */
def evens[A](l: List[A]): List[A] = l match {
    case Nil => Nil 
    case x :: xs => x :: evens(xs.tail) // tail of an empty list is an error 
}

/* Returns true if all operations are addition, false otherwise.
What is expected if the expression is just a number? */
def allplus(e: Expr): Boolean = e match {
    case Num(n) => true
    case Plus(e1, e2) => true && (allplus(e1) && allplus(e2))
    case Times(e1, e2) => false
}

/* Constructs a list containing all numerical 
constants in an expression. */
def consts(e: Expr): List[Int] = e match {
    case Num(n) => n :: Nil 
    case Plus(e1, e2) => consts(e1) ++ consts(e2)
    case Times(e1, e2) => consts(e1) ++ consts(e2)
}

/* Reverses thee order of all multiplication operations. */
def revtimes(e: Expr): Expr = e match {
    case Num(n) => Num(n)
    case Plus(e1, e2) => Plus(revtimes(e1), revtimes(e2))
    case Times(e1, e2) => Times(revtimes(e2), revtimes(e1))
}

/* Converts an expression into a fully parenthesised string. */
def printExpr(e: Expr): String = e match {
    case Num(n) => n.toString
    case Plus(e1, e2) => "(" + printExpr(e1) + " + " + printExpr(e2) + ")"
    case Times(e1, e2) => "(" + printExpr(e1) + " * " + printExpr(e2) + ")"
}



