sealed trait Either[+E, +A]:
  // Ex. 4.6
  def map[B](f: A => B): Either[E, B] = this match
    case Left(e) => Left(e) 
    case Right(a) => Right(f(a))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = 
    this match
      case Left(e) => Left(e) 
      case Right(a) => f(a) 

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = 
    this match
      case Left(_) => b
      case _ => this

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    this.flatMap((aa) => b.map((bb) => f(aa, bb)))



case class Left[+E] (value: E) extends Either[E, Nothing]
case class Right[+A] (value: A) extends Either[Nothing, A]


// Tests
// 4.6

val a: Either[String, Int] = Right(3)
val b: Either[String, Int] = Left("a string") 
val f = (x: Int) => x + 2
val g = (x: Int) => Right(x + 2) 
val add = (x: Int, y: Int) => x + y

a.map(f)
b.map(f) 
a.flatMap(g)
b.flatMap(g) 
a.orElse(b) 
b.orElse(a) 
a.map2(b)(add) 
b.map2(a)(add) 
a.map2(a)(add) 


// Examples

def mean(xs: List[Double]): Either[String, Double] = 
  if (xs.isEmpty) Left("mean of empty list!")
  else Right(xs.sum/xs.length)

mean(List(3.0, 2.0, 1.0))
mean(List())

def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
  try Right(x/y) 
  catch { case e: Exception => Left(e) } 

safeDiv(3, 0) 

def Try[A](a: => A): Either[Exception, A] = 
  try Right(a)
  catch { case e: Exception => Left(e) }





