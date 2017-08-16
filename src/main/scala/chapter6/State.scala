package chapter6


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (clean, nextRand) = rng.nextInt
    (clean & ~0>>>1, nextRand)

  }

  def double(rng: RNG): (Double, RNG) = {
    val(clean, nextRand) =  rng.nextInt
    (clean/(Int.MaxValue.toDouble + 1), nextRand)
  }

  def double1(rng:RNG): (Double, RNG) = {
    map(int)(_/Int.MaxValue.toDouble +1)(rng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (nextI, rand1) = rng.nextInt
    val (nextD, rand2) = double(rand1)
    ((nextI, nextD), rand2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((nextI, nextD), rand) = intDouble(rng)
    ((nextD, nextI), rand)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0){
      (List(), rng)
    }else{
      val (i, rand) = rng.nextInt
      val (tail, r1) = ints(count-1)(rand)
      (i :: tail, r1)
    }
  }

  def ints2(count: Int)(rng: RNG): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rand => {
      val (a, rand1) = ra(rand)
      val (b, rand2) = rb(rand1)
      (f(a,b), rand2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit[List[A]](List()))((item, acc) => map2(item, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (inner, rng2) = f(rng)
      g(inner)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(num => {
      val mod = num % n
      if(num + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    })

  def map1[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x => unit(f(x)))

  def map2_2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = new State(s => {
    val (item, state) = run(s)
    f(item).run(state)
  })
}

object State{
  type Rand[A] = State[RNG, A]
  def unit[S,A](a: A): State[S,A] = State((s:S) => (a, s))
  def sequence[S,A](fs: List[State[S,A]]): State[S, List[A]] =
    fs.foldRight(unit[S,List[A]](Nil))((item, acc) => item.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  import State._

  def update = (i: Input) => (m: Machine) => (i,m) match {
    case (_, Machine(_, 0, _)) => m
    case (Coin, Machine(false, _, _)) => m
    case (Turn, Machine(true, _, _)) => m
    case (Coin, Machine(true, ca, co)) => Machine(false, ca, co + 1)
    case (Turn, Machine(false, ca, co)) => Machine(true, ca - 1, co)

  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(x => modify(update(x))))
    s <- get
  } yield(s.candies, s.coins)
}
