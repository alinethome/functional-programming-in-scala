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

  // Ex. 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] = 
    foldRight[Stream[A]](Empty)((h, t) => 
        if p(h) then cons(h, t)
        else t)

  def append[B >: A](bs: Stream[B]): Stream[B] = 
    foldRight(bs)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight[Stream[B]](Empty)((h, t) => f(h).append(t))

  // Ex. 5.13
  def mapUF[B](f: A => B): Stream[B] =
    unfold(this)(as => 
        as match 
          case Empty => None
          case Cons(h, t) => Some((f(h()), t())))

  def takeUF(n: Int): Stream[A] = 
    unfold((this, n)){
      case (Empty, _)      => None
      case (_, 0)          => None
      case (Cons(h, t), m) => Some(h(), (t(), m - 1))
    }

  def takeWhileUF(p: A => Boolean): Stream[A] = 
    unfold(this){
      case Empty      => None
      case Cons(h, t) => 
        val head = h()

        if p(head) then Some(head, t()) 
        else None
    }

  def zipWith[B,C](bs: Stream[B])(f: (A, B) => C): Stream[C] = 
    unfold((this, bs)){
      case (Cons(ha, ta), Cons(hb, tb)) => 
        Some(f(ha(), hb()), (ta(), tb()))
      case _                            => None
    }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, bs)){
      case (Empty, Empty)               => None
      case (Cons(ha, ta), Empty)        => 
        Some((Some(ha()), None), (ta(), Empty))
      case (Empty, Cons(hb, tb))        =>
        Some((None, Some(hb())), (Empty, tb()))
      case (Cons(ha, ta), Cons(hb, tb)) => 
        Some((Some(ha()), Some(hb())), (ta(), tb()))
    }

  def startsWith[A](s2: Stream[A]): Boolean = 
    this.zipAll(s2)
        .foldRight(true)((pair, acc) => pair match 
              case (None, Some(_))      => false
              case (_, None)            => true
              case (Some(a1), Some(a2)) => acc && (a1 == a2)
        )

  def tails: Stream[Stream[A]] = 
    unfold(this)(s => s match 
      case Empty      => None
      case Cons(h, t) => Some((s, t()))).append(Stream(Empty))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = 
    this.foldRight((z, Stream(z)))((a, acc) => 
        val b1 = acc._1
        val b2 = f(a, b1) 
        val bs = acc._2

        (b2, cons(b2, bs)))._2

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

  // ex 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // ex 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  // ex 5.10
  def fibs: Stream[Int] = 
    def fibs(m: Int, n: Int): Stream[Int] =
      cons(n, fibs(n, n+m))

    fibs(0, 1) 

  // ex 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match 
    case None         => empty
    case Some((a, s)) => cons(a, unfold(s)(f))

  // ex 5.12
  def onesUF: Stream[Int] = unfold(1)(n => Some(1, 1))

  def constantUF[A](a: A): Stream[A] = unfold(a)(c => Some(c, c))

  def fromUF(a: Int): Stream[Int] = unfold(a)(n => Some(n, n + 1))

  def fibsUF: Stream[Int] = unfold((0, 1))((m, n) => Some(n, (n, n+m)))

// Tests

val stream = Stream(1, 2, 3) 
val stream2 = Stream(1, 3, 4) 
val empty = Stream()
val inf: Stream[Int] = Stream.cons(1, inf)
val multiplesOf2LessThan10 = (n: Int) => if (2*n >= 10) then None 
                                         else Some((n*2, n+1))

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
stream.map(n => n + 1).toList
stream.filter(n => n % 2 == 0).toList
stream2.filter(n => n % 2 == 1).toList
stream.append(stream2).toList
stream.flatMap(n => Stream(n)).toList
inf.take(5).toList
Stream.constant(5).take(5).toList
Stream.from(3).take(5).toList
Stream.fibs.take(5).toList
Stream.unfold(1)(multiplesOf2LessThan10).toList
Stream.onesUF.take(5).toList
Stream.constantUF('a').take(7).toList
Stream.fromUF(3).take(3).toList
Stream.fibsUF.take(10).toList
stream.mapUF(n => n + 1).toList
stream.takeUF(3).toList
stream.takeUF(5).toList
stream.takeUF(2).toList
stream.takeUF(0).toList
stream.takeUF(1).toList
stream2.takeWhileUF(n => n % 2 == 1).toList
stream2.takeWhileUF(n => n % 2 == 0).toList
stream.zipWith(stream2)((_,_)).toList
stream.zipAll(Stream.fibs.take(2))
      .toList
stream.startsWith(stream2)
stream.startsWith(stream2.take(1))
stream.tails.map(_.toList).toList
stream.scanRight(0)(_ + _).toList
