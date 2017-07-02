/**
  * Created by joe on 31/05/17.
  */
object Scratch extends App {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, r) = s(rng)
      (f(a), r)
    }

  val double: Rand[Double] = map(int) {
    i => 0.5 - i / (2 * Int.MinValue.toDouble)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng1 => {
      val (a, rng2) = ra(rng1)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil =>
      rng => (Nil, rng)
    case x :: xs =>
      rng =>
        val (a, r1) = x(rng)
        val (as, r2) = sequence(xs)(r1)
        (a :: as, r2)
  }
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f,acc) => map2(f,acc)(_::_))

  def flatMap[A,B](f:Rand[A])(g: A => Rand[B]):Rand[B] = rng => {
    val (a,r) = f(rng)
    g(a)(r)
  }

  def mapp[A,B](f:Rand[A])(g:A=>B): Rand[B] = flatMap (f)(g andThen unit)
  def mapp2[A,B,C](fa:Rand[A],fb:Rand[B])(g:(A,B)=>C):Rand[C] = flatMap(fa)(a => mapp(fb)(b => g(a,b)))

  case class State[S,+A](run:S=>(A,S)) {
    def flatMap[B](f:A=>State[S,B]):State[S,B] = State{
      s => {
        val (a,s2)= run(s)
        f(a).run(s2)
      }
    }
    def map[B](f:A=>B):State[S,B] = flatMap(a => State.unit(f(a)))
    def map2[B,C](sb:State[S,B])(f:(A,B) =>C): State[S, C] = for {
      a <- this
      b <- sb
    } yield f(a,b)
  }

  object State {
    def unit[S,A](a:A):State[S,A] = State(s=>(a,s))
    def sequence[S,A](sas:List[State[S,A]]):State[S,List[A]] = sas.foldRight(unit[S,List[A]](Nil))((s,sl)=>s.map2(sl)(_::_))
  }

  def get[S]: State[S,S] = State(s => (s,s))
  def set[S](s:S): State[S,Unit] = State(_ =>((),s))
  def modify[S](f:S=>S):State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked:Boolean, candies:Int, coins:Int)

  def update(input:Input)(machine:Machine):Machine = (input,machine) match {
    case (_,Machine(_,0,_)) => machine
    case (Coin,Machine(true,i,j)) => Machine(false,i,j+1)
    case (Turn,Machine(false,i,j)) => Machine(true,i-1,j)
    case _ => machine
  }

  def simulateMachine(inputs: List[Input]):State[Machine,(Int,Int)] = for {
    _ <- State.sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)

  simulateMachine(List(Coin,Turn)).run(Machine(true,2,1))
  def isFinished : State[Machine,Boolean] = get.map(m => m.candies == 0)
  def nextInput : State[Machine,Input] = get map {
    case Machine(true,_,_) => Coin
    case _ => Turn
  }

  def useNextInput : State[Machine,Unit] = for {
    input <- nextInput
    _ <- modify(update(input))
  } yield ()

  def useTillFinished: State[Machine,Unit] = for {
    b <- isFinished
    _ <- if (b) State.unit[Machine,Unit](()) else for {
      _ <- useNextInput
      _ <- useTillFinished
    } yield ()
  } yield ()

  println(useTillFinished.run(Machine(true,5,2)))











}
