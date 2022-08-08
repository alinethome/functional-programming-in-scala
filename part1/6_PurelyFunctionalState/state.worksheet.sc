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



