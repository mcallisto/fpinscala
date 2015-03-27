package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService ⇒ Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // `unit` is represented as a function that returns a `UnitFuture`,
  // which is a simple implementation of `Future` that just wraps a constant value.
  // It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled.
  // Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = (es: ExecutorService) ⇒ UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread,
  // in accord with our design choice of having `fork` be the sole function in the API
  // for controlling parallelism. We can always do `fork(map2(a,b)(f))`
  // if we want the evaluation of `f` to occur in a separate thread.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) ⇒ C): Par[C] =
    (es: ExecutorService) ⇒ {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
      // This implementation of `map2` does _not_ respect timeouts,
      // and eagerly waits for the returned futures.
      // This means that even if you have passed in "forked" arguments,
      // using this map2 on them will make them wait.
      // It simply passes the `ExecutorService` on to both `Par` values,
      // waits for the results of the Futures `af` and `bf`, applies `f` to them,
      // and wraps them in a `UnitFuture`. In order to respect timeouts,
      // we'd need a new `Future` implementation that records the amount of time spent evaluating `af`,
      // then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) Par.unit(as.headOption getOrElse 0)
    else {
      //      println("p " +as)
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  def sumSerial(as: IndexedSeq[Int]): Int =
    if (as.size <= 1) as.headOption getOrElse 0
    else {
      //      println(as)
      val (l, r) = as.splitAt(as.length / 2)
      sumSerial(l) + sumSerial(r)
    }

  // This is the simplest and most natural implementation of `fork`, but there are some problems with it
  // --for one, the outer `Callable` will block waiting for the "inner" task to complete.
  // Since this blocking occupies a thread in our thread pool,
  // or whatever resource backs the `ExecutorService`,
  // this implies that we're losing out on some potential parallelism.
  // Essentially, we're using two threads when one should suffice.
  // This is a symptom of a more serious problem with the implementation,
  // and we will discuss this later in the chapter.
  def fork[A](a: ⇒ Par[A]): Par[A] =
    es ⇒ es.submit(new Callable[A] {
      def call = a(es).get
    })

  // EXERCISE 4: This API already enables a rich set of operations.
  // Here's a simple example: using async, write a function to convert any function A => B
  // to one that evaluates its result asynchronously
  def asyncF[A, B](f: A ⇒ B): A ⇒ Par[B] = a ⇒ fork(unit(f(a)))

  def map[A, B](pa: Par[A])(f: A ⇒ B): Par[B] =
    map2(pa, unit(()))((a, _) ⇒ f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  // EXERCISE 5: Implement product and map as primitives, then define map2 in terms of them.
  // map2 is actually doing two things—
  // it is creating a parallel computation that waits for the result of two other computations
  // and then it is combining their results using some function.
  // We could split this into two functions, product and map

  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] =
    (es: ExecutorService) ⇒ {
      val af = fa(es)
      val bf = fb(es)
      UnitFuture((af.get, bf.get))
    }

  def mapO[A, B](fa: Par[A])(f: A ⇒ B): Par[B] =
    (es: ExecutorService) ⇒ {
      val af = fa(es)
      UnitFuture(f(af.get))
    }

  def map2O[A, B, C](a: Par[A], b: Par[B])(f: (A, B) ⇒ C): Par[C] =
    mapO(product(a, b))(_ match { case (aa, bb) => f(aa, bb) })

  // EXERCISE 6: Note that we could always just write parMap as a new primitive.
  // See if you can implement it this way.
  // Remember that Par[A] is simply an alias for ExecutorService => Future[A].
  // Unlike map2, which combines two parallel computations, parMap (let's
  // call it) needs to combine N parallel computations. Still, it seems like this should
  // somehow be expressible.
  def parMapO[A, B](l: List[A])(f: A ⇒ B): Par[List[B]] =
    (es: ExecutorService) ⇒ UnitFuture(l.map(f))

  // EXERCISE 7: Let's write sequence. No additional primitives are required.  
  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(Nil): Par[List[A]])((x, y) => map2(x, y)(_ :: _))

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence(fbs)
  }

  // EXERCISE 8: Implement parFilter, which filters elements of a list in parallel.
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val pars: List[Par[List[A]]] =
      l map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

  def max(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) Par.unit(as.headOption getOrElse Int.MinValue)
    else {
      //      println("p " +as)
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(Par.fork(max(l)), Par.fork(max(r)))((ll, rr) => if (ll > rr) ll else rr)
    }

  def fold[A](as: IndexedSeq[A])(z: A)(f: (A, A) ⇒ A): Par[A] = as match {
    case Vector()      ⇒ Par.unit(z)
    case v +: Vector() ⇒ Par.unit(v)
    case _ ⇒
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(Par.fork(fold(l)(z)(f)), Par.fork(fold(r)(z)(f)))(f)
  }
  
  def maxF(as: IndexedSeq[Int]): Par[Int] =
    fold(as)(Int.MinValue)((a, b) ⇒ if (a > b) a else b)
    
  def sumF(as: IndexedSeq[Int]): Par[Int] =
    fold(as)(0)(_ + _)

  def fold2[A, B](as: List[A])(z: B)(e: A ⇒ B)(f: (B, B) ⇒ B): Par[B] = as match {
    case Nil      ⇒ Par.unit(z)
    case v +: Nil ⇒ Par.unit(e(v))
    case _ ⇒
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(Par.fork(fold2(l)(z)(e)(f)), Par.fork(fold2(r)(z)(e)(f)))(f)
  }
  
  def totalWordsF(as: List[String]): Par[Int] = {
    def countWords(s: String): Int = s.split("\\W+").size
      
    fold2(as)(0)(countWords)(_ + _)
  }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) ⇒ D): Par[D] = {
    val ab = map2(a, b)((aa, bb) ⇒ (aa, bb))
    map2(ab, c)((ab, cc) ⇒ f(ab._1, ab._2, cc))
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) ⇒ E): Par[E] = {
    val abc = map3(a, b, c)((aa, bb, cc) ⇒ (aa, bb, cc))
    map2(abc, d)((abc, dd) ⇒ f(abc._1, abc._2, abc._3, dd))
  }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) ⇒ F): Par[F] = {
    val abcd = map4(a, b, c, d)((aa, bb, cc, dd) ⇒ (aa, bb, cc, dd))
    map2(abcd, e)((abcd, ee) ⇒ f(abcd._1, abcd._2, abcd._3, abcd._4, ee))
  }
  
  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: ⇒ Par[A]): Par[A] =
    es ⇒ fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es ⇒
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {

  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
