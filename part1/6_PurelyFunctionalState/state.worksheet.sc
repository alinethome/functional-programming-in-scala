case class State[S, +A](run: S => (A, S)):
  import State._

  // ex 8.10
  def flatMap[B](g: A => State[S, B]): State[S, B] = 
    val getNext = (s: S) => 
      val (a, s2) = run(s)
      g(a).run(s2)

    State(getNext) 

  def map[B](f: A => B): State[S, B] = 
    flatMap(a => unit(f(a)))

  def map[B, C](sb: State[S, B])(g: (A, B) => C): State[S, C] = 
    flatMap(a => sb.map(b => g(a, b)))

  
object State:
  // ex 8.10
  def unit[S, A](a: A): State[S, A] = 
    State(s => (a, s))

  def sequence[A, S](sas: List[State[S, A]]): State[S, List[A]] = 
    def go(sas: List[State[S, A]], as: List[A], s: S): (List[A], S) = 
      sas match 
        case List() => (as.reverse, s) 
        case h :: t => 
          val (a, s2) = sas.head.run(s) 
          go(sas.tail, a :: as, s2)

    State(go(sas, List(), _))

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()


sealed trait Input 
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) 

def update(input: Input)(machine: Machine): Machine = input match 
  case Coin if machine.locked && machine.candies > 0 => 
    Machine(false, machine.candies, machine.coins + 1) 
  case Turn if !machine.locked && machine.candies > 0 => 
    Machine(true, machine.candies - 1, machine.coins + 1) 
  case _ => machine


// def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = 
  


// test

val m = Machine(true, 5, 0) 
val inputs = List(Coin, Turn, Coin, Coin, Turn, Turn) 
val transformations = inputs.map(update(_))
val init: State[Machine, (Int, Int)] = State(m => ((m.candies, m.coins), m))
// val first = init.flatMap(

// val g = (pair) => 

// transformations.foldRight(init(m))((t, s) =>
//     s.flatMap()
//     ) 

update(Coin)(m) 

init.run(m) 

State.modify(update(Coin))


