import scala.annotation.tailrec

sealed trait Stream[+A]:
  def headOption: Option[A] = this match 
    case Empty => None
    case Cons(h, t) => Some(h())

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
stream.toListRec
stream.toList
stream.take(3).toList
stream.take(2).toList
stream.take(4).toList
stream.drop(0).toList
stream.drop(2).toList
stream.drop(4).toList

