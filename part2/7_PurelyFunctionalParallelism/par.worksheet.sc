import java.util.concurrent._
import java.util.concurrent.TimeUnit._

def sum(ints: IndexedSeq[Int]): Par[Int] = 
  if (ints.size <= 1) 
    Par.unit(ints.headOption getOrElse 0)
  else
    val (l, r) = ints.splitAt(ints.length/2) 
    Par.map2(sum(l), sum(r))(_ + _) 


type Par[A] = ExecutorService => Future[A] 

object Par: 
  def unit[A](a: => A): Par[A] = (ex: ExecutorService) => UnitFuture(a) 
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  def fork[A](a: => Par[A]): Par[A] = 
    es => es.submit(new Callable[A]: 
      def call = a(es).get)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def map[A, B](a: Par[A])(f: A => B): Par[B] = 
    map2(a, unit(()))((a, _) => f(a))
  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted) 

  // ex 7.1
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = 
    (es: ExecutorService) => 
      val af = a(es) 
      val bf = b(es) 
      UnitFuture(f(af.get, bf.get))

  // ex 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))


  private case class UnitFuture[A](get: A) extends Future[A]: 
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false


// Tests
val nums = Vector(11, 34, 2, -1, -3) 
val es = Executors.newFixedThreadPool(2) 

Par.unit(3) 
Par.unit(3)(es).get(100, MILLISECONDS) 
sum(nums)(es).get(100, MILLISECONDS)  
