trait RNG:
  def nextInt: (Int, RNG)

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) = 
    val newSeed =  (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

type Rand[+A] = RNG => (A, RNG) 
val int: Rand[Int] = _.nextInt
def unit[A](a: A): Rand[A] = rng => (a, rng) 
def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  rng => 
    val (a, rng2) = s(rng) 
    (f(a), rng2)



// ex 6.1
def nonNegativeInt(rng: RNG) = 
  val (n: Int, rng2: RNG) = rng.nextInt

  if (n < 0) then (-n + 1, rng2) 
  else (n, rng2) 
  
// ex 6.2
def double(rng: RNG) = 
  val (n: Int, rng2: RNG) = nonNegativeInt(rng) 
  val double = n / (Int.MaxValue.toDouble + 1)
  
  (double, rng2) 

// ex 6.3
def intDouble(rng: RNG): ((Int, Double), RNG) = 
  val (i, rng2) = rng.nextInt
  val (d, rng3) = double(rng2) 

  ((i, d), rng3) 

def doubleInt(rng: RNG): ((Double, Int), RNG) = 
  val (d, rng2) = double(rng) 
  val (i, rng3) = rng2.nextInt

  ((d, i), rng3) 

def double3(rng: RNG): ((Double, Double, Double), RNG) = 
  val (d1, rng2) = double(rng)
  val (d2, rng3) = double(rng2)
  val (d3, rng4) = double(rng3)

  ((d1, d2, d3), rng4)

// ex 6.4
def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
  def loop(n: Int, res: (List[Int], RNG)): (List[Int], RNG) = 
    if n <= 0 then res
    else 
      val (is, rng2) = res
      val (i, rng3) = rng2.nextInt
      loop(n - 1, (i :: is, rng3))

  loop(count, (List(), rng))

// ex 6.5
def doubleMap: Rand[Double] = 
  map(int)(_/(Int.MaxValue.toDouble + 1))

// ex 6.6
def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
  rng => 
    val (a, rng2) = ra(rng) 
    val (b, rng3) = rb(rng2) 

    (f(a, b), rng3) 

def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
  map2(ra, rb)((_, _)) 

val randIntDouble: Rand[(Double, Int)] = both(double, int) 
val randDoubleInt: Rand[(Int, Double)] = both(int, double) 
    
// ex 6.7
def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
  def go(fs: List[Rand[A]], as: List[A], r: RNG): (List[A], RNG) = 
    fs match
      case List() => (as, r) 
      case h :: t =>
        val (a, r2) = h(r)
        go(t, a :: as, r2)

  rng => go(fs, List(), rng)

def intsSeq(count: Int): Rand[List[Int]] = 
  sequence(List.fill(count)(int))



// Tests
val r = SimpleRNG(42) 

val nni = nonNegativeInt(r)
nonNegativeInt(nni._2)
double(r) 
intDouble(r) 
doubleInt(r) 
double3(r) 
ints(3)(r) 
doubleMap(r) 
both(doubleMap, doubleMap)(r) 
intsSeq(3)(r) 
