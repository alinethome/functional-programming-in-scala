trait Monoid[A]: 
  def op(a1: A, a2: A): A
  def zero: A


val stringMonoid = new Monoid[String]:
  def op(a1: String, a2: String): String = a1 + a2
  def zero: String = ""

// ex 10.1
val intAddition = new Monoid[Int]: 
  def op(m: Int, n: Int): Int = m + n 
  def zero: Int = 0 

val intMultiplication = new Monoid[Int]: 
  def op(m: Int, n: Int): Int = m*n
  def zero: Int = 1

val booleanOr = new Monoid[Boolean]: 
  def op(b1: Boolean, b2: Boolean): Boolean = b1 || b2
  def zero: Boolean = false

val booleanAnd = new Monoid[Boolean]: 
  def op(b1: Boolean, b2: Boolean): Boolean = b1 && b2
  def zero: Boolean = true

// ex 10.2
// def optionMonoid[A]: Monoid[Option[A]] = 
//   new Monoid[Option[A]]: 
//     def op(o1: Option[A], o2: Option[A]): Option[A] = (o1, o2) match
//       case (Some(a), _) => Some(a) 
//       case (None, o)    => o
//     def zero: Option[A] = None 

def optionMonoid[A]: Monoid[Option[A]] = 
  new Monoid[Option[A]]: 
    def op(o1: Option[A], o2: Option[A]): Option[A] = 
      o1 orElse o2
    def zero: Option[A] = None 

// ex 10.3
def endoMonoid[A]: Monoid[A => A] = 
  new Monoid[A => A]: 
    def op(f: A => A, g: A => A): A => A = 
      g compose f
    def zero: A => A = identity

def concatenate[A](as: List[A], m: Monoid[A]): A = 
  as.foldLeft(m.zero)(m.op)

// ex 10.5
def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = 
  as.foldLeft(m.zero)((acc, a) => m.op(acc, f(a)))

// ex 10.7
def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = 
  if (v.length <= 1)
    if v.length == 0 then m.zero else f(v(0))
  else
    val (l, r) = v.splitAt(v.length/2)
    m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))

sealed trait WC 
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

// ex 10.10
val wcMonoid: Monoid[WC] = 
  new Monoid[WC]: 
    def zero: WC = Stub("") 
    def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match
      case (Stub(s1), Stub(s2))                 => Stub(s1 + s2) 
      case (Stub(s), Part(l, w, r))             => Part(s + l, w, r)
      case (Part(l, w, r), Stub(s))             => Part(l, w, r + s)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => 
        val w3 = if l2 + r1 == "" then 0 else 1
        Part(l1, w1 + w2 + w3, r2)

def countWords(s: String): Int =
  val wc: WC = foldMapV(s, wcMonoid)(c => 
      if c.isWhitespace then Part("", 0, "") else Stub(c.toString))
  
  wc match 
  case Stub(s) => if s.isEmpty then 0 else 1
  case Part(l, wc2, r) => 
    val wl = if l.isEmpty then 0 else 1
    val wr = if r.isEmpty then 0 else 1
    wl + wc2 + wr

// Tests
stringMonoid.zero
stringMonoid.op("a", "b")
intAddition.zero
intAddition.op(1, 2)
intMultiplication.zero
intMultiplication.op(1, 2)
booleanOr.zero
booleanOr.op(true, false)
booleanAnd.zero
booleanAnd.op(true, false)
optionMonoid.op(None, Some(3))
optionMonoid.op(Some(4), Some(3))
optionMonoid.op(Some(4), None)
optionMonoid.op(None, None)
val f = (n: Int) => n*2
val g = (n: Int) => n+1
endoMonoid[Int].op(f, g)(3) 
val test = "this is a test string" 
countWords(test)
