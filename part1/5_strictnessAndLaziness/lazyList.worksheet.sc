import scala.annotation.tailrec

sealed trait Stream[+A]:
  import Stream._

  def headOption: Option[A] = this match 
    case Empty => None
    case Cons(h, t) => Some(h())

  def foldRight[B](z: => B)(f: (A, => B) => B): B = 
    this match 
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)

  // Ex. 5.1
  def toListRec: List[A] = this match
    case Empty    => Nil
    case Cons(h, t) => h() :: t().toList

  def toList: List[A] = 
    @tailrec
    def loop(sas: Stream[A], as: List[A]): List[A] = 
      sas match
        case Empty => as
        case Cons(h, t) => loop(t(), h() :: as)

    loop(this, Nil).reverse

  // Ex. 5.2
  def take(n: Int): Stream[A] = 
    if n == 0 then return Empty
    else 
      this match 
        case Empty => Empty
        case Cons(h, t) => Cons(h, () => t().take(n - 1))

  def drop(n: Int): Stream[A] = 
    if n == 0 then return this 
    else 
      this match 
        case Empty => Empty
        case Cons(_, t) => t().drop(n - 1) 

  // Ex. 5.3
  def takeWhile(p: A => Boolean): Stream[A] = 
    this match
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _                    => Empty

  // Ex. 5.4
  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((h, acc) => p(h) && acc)

  // Ex. 5.5 
  def takeWhileFR(p: A => Boolean): Stream[A] = 
    foldRight[Stream[A]](Empty)((h, acc) => if p(h) then cons(h, acc) else Empty)

  // Ex. 5.6
  def headOptionFR: Option[A] = foldRight[Option[A]](None)((h, acc) => Some(h))
      

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream: 
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tl)

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


// Tests

val stream = Stream(1, 2, 3) 
val stream2 = Stream(1, 3, 4) 
val empty = Stream()

stream.toListRec
stream.toList
stream.take(3).toList
stream.take(2).toList
stream.take(4).toList
stream.drop(0).toList
stream.drop(2).toList
stream.drop(4).toList
stream2.takeWhile((n) => n % 2 == 1).toList
stream.takeWhile((n) => n % 2 == 1).toList
stream.takeWhile((n) => n % 2 == 0).toList
stream.takeWhile((n) => n % 1 == 0).toList
stream.forAll((n) => n % 2 == 0)
stream.forAll((n) => n % 1 == 0)
stream2.takeWhileFR((n) => n % 2 == 1).toList
stream.takeWhileFR((n) => n % 2 == 1).toList
stream.takeWhileFR((n) => n % 2 == 0).toList
stream.takeWhileFR((n) => n % 1 == 0).toList
stream.headOptionFR
empty.headOptionFR
