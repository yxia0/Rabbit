/** Assignment1 of EPL 2022 
 *  @author Youning Xia
 */

import scala.collection.immutable.ListMap

/* Part 2 */

def incr(x: Int): Int = x + 1
def double(x: Int): Int = x + x
def square(x: Int): Int = x * x

def factorial(n: Int): Int = 
  if (n == 0) {1} else {n * factorial(n-1)}


def power(x: Int, n: Int): Int =
  if (n == 0) {1} else {x * power(x,n-1)}

def factorial1(n: Int): Int = {
  val m = n-1 ; if (n == 0) {1} else {n * factorial1(m)}
}


def factorial2(n: Int): Int = {
  val m = n-1;
  if (n == 0) {1} else {n * factorial2(m)}
}


def factorial3(n: Int): Int = {
  val m = n-1;
  if (n == 0) {
    return 1;
  } else {
    return n * factorial3(m);
  }
}

/* Exercise 1 */
def p(x: Int, y:Int): Int = {
  val a = power(x, 2);
  val b = 2 * x * y;
  val c = power(y, 3);
  val d = -1;
  a + b + c + d
}

/* Exercise 2 */
def sum(n: Int): Int = {
  val m = n-1;
  if (n == 0) {0} else {n + sum(m)}
}

/* Part 3 */

/* Exercise 3 */
def cycle(q:(Int,Int,Int)): (Int,Int,Int) = {
  val first = q._1;
  val sec = q._2;
  val third = q._3;
  (sec, third, first)
}
// alternative form:
// def cycle(x: Int,y: Int,z: Int): (Int,Int,Int) = sys.error("todo")

/* Part 4 */


def nameFromNum(presidentNum: Int): String = presidentNum match {
  case 41 => "George H. W. Bush"
  case 42 => "Bill Clinton"
  case 43 => "George W. Bush"
  case 44 => "Barack Obama"
  case 45 => "Donald J. Trump"
  case n => "I don't know who president number " + n + " is"
}

def numFromName(presidentName: String): Int = presidentName match {
  case "George H. W. Bush" => 41
  case "Bill Clinton" => 42
  case "George W. Bush" => 43
  case "Barack Obama" => 44
  case "Donald J. Trump" => 45
}

/* Exercise 4 */
def suffix(n: Int): String = n match {
  case _ if n % 100 == 11 => "th"
  case _ if n % 100 == 12 => "th"
  case _ if n % 100 == 13 => "th"
  case _ if n % 10 == 1 => "st"
  case _ if n % 10 == 2 => "nd"
  case _ if n % 10 == 3 => "rd"
  case _ => "th"
}


abstract class Colour
case class Red() extends Colour
case class Green() extends Colour
case class Blue() extends Colour
case class Black() extends Colour

/* Exercise 5 */
def favouriteColour(c: Colour): Boolean = c match { 
  case Black() => true
  case _ => false
}


abstract class Shape
case class Circle(r: Double, x: Double, y: Double) extends Shape
case class Rectangle(llx: Double, lly: Double, w:Double, h:Double) extends Shape

def center(s: Shape): (Double,Double) = s match {
  case Rectangle(llx,lly,w,h) => (llx+w/2, lly+h/2)
  case Circle(r,x,y) => (x,y)
}

/* Exercise 6 */
def boundingBox(s: Shape): Rectangle = s match {
  case Rectangle(llx,lly,w,h) => Rectangle(llx, lly, w, h)
  case Circle(r, x, y) => Rectangle(x-r, y-r, 2*r, 2*r)
}

/* Exercise 7 */

/* Returns true if the vertical dimension of two rectangles overlaps.
  Otherwise returns false */
def vOverlap(r1: Rectangle, r2: Rectangle): Boolean = {
  val r1Right = r1.llx + r1.w;
  val r1Left = r1.llx;
  val r2Right = r2.llx + r2.w;
  val r2Left = r2.llx;
  if (r1Right <=r2Left | r2Right <=r1Left) {false} else {true}
}

/* Returns true if the horizontal dimension of two rectangles overlaps.
 Otherwise returns false */
def hOverlap(r1: Rectangle, r2: Rectangle): Boolean = {
  val r1Top = r1.lly + r1.h;
  val r1Bottom = r1.lly;
  val r2Top= r2.lly + r2.h;
  val r2Bottom = r2.lly;
  if (r1Top <=r2Bottom | r2Top <=r1Bottom) {false} else {true}
}

def mayOverlap(s1: Shape, s2: Shape):Boolean = {
  val bbs1 = boundingBox(s1);
  val bbs2 = boundingBox(s2);
  val vertOverlap = vOverlap(bbs1, bbs2);
  val horizOverlap = hOverlap(bbs1, bbs2);
  if (vertOverlap == true | horizOverlap == true) {true} else {false}
}

/* Part 5 */

val anonIncr = {(x: Int) => x+1} // anonymous version of incr
val anonAdd = {(x: Int) => {(y: Int) => x + y}}

/* Exercise 8 */
def compose1[A, B, C](f: A => B, g: B => C)(x: A) = {
  val fx = f(x);
  g(fx)
}


/* Exercise 9 */
def compose[A, B, C](f: A => B, g: B => C) = {(x: A) => g(f(x))}

/* Exercise 10 */
def e1(x: Int): String = x match {
  case 1 => "Bingo"
  case _ => "Please come next time"
}
def e2(y: String): Boolean = y == "Bingo"  

/* Exercise 10 */

// scala> compose(e1, e2)
// res75: Int => Boolean = $Lambda$1498/0x00000008015a0978@260ed754

// scala> compose(e2, e1)
// <console>:18: error: type mismatch;
//  found   : Int => String
//  required: Boolean => String
//        compose(e2, e1)

def isEmpty[A](l: List[A]) = l match { 
  case Nil => true
  case x :: y => false
}


/* Exercise 11 */
def map[A, B](f: A => B, l: List[A]): List[B] = l match {
  case Nil => Nil
  case x :: xs => f(x) :: map(f, xs)  
}

/* Exercise 12 */
def filter[A](f: A => Boolean, l: List[A]): List[A] = l match {
  case Nil => Nil
  case x :: xs => if (f(x)) {x :: filter(f, xs)} else {filter(f, xs)}
}

/* Exercise 13 */
def reverse[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case x :: xs => reverse(xs) :+ x
}


/* Part 6 */

def empty[K,V]: List[(K,V)] = List()

/* Exercise 14 */
def lookup[K, V](m: List[(K, V)], k: K): V = m match {
  case Nil => sys.error("No matching key is found")
  case x :: xs => if (x._1 == k) {x._2} else {lookup(xs, k)}
}

/* Exercise 15 */
def update[K, V](m: List[(K, V)], k: K, v: V): List[(K, V)] = m match {
  case Nil => (k, v) :: Nil 
  case x :: xs => if (x._1 == k) {(k, v) :: xs} else {x :: update(xs, k, v)}
}

/* Exercise 16 */
def keys[K,V](m: List[(K,V)]): List[K] = m match {
  case Nil => Nil 
  case x :: xs => x._1 :: keys(xs)
}

/* Exercise 17 */
val presidentListMap = ListMap(41 -> "George H. W. Bush", 
                               42 -> "Bill Clinton", 
                               43 -> "George W. Bush", 
                               44 -> "Barack Obama",
                               45 -> "Donald J. Trump")

/* Exercise 18 */
def map12_withUpdate = {
  val empty = ListMap[Int, String]();
  empty + (1 -> "a", 2 -> "b")
}

/* Exercise 19 */
def list2map[K,V](l: List[(K,V)]): ListMap[K,V] = {
  val map = ListMap[K, V]();
  l match {
    case Nil => map
    case x :: xs => list2map(xs) + (x._1 -> x._2)
  }
}

/* Exercise 20 */
def election(votes: List[String]): ListMap[String,Int] = {
  var m = ListMap[String, Int]();
  for (name <- votes) {
    if (m.contains(name)) {
      m = m + (name -> (m(name) + 1))
    } else {
      m = m + (name -> 1)
    }
  }
  m
}