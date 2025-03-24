package u04lab

import u03.Sequences.*
import Sequence.*
import u03.Optionals.Optional.Just
import u03.extensionmethods.Optionals.*
import Optional.*
import u03.Optionals

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:
  trait Traversable[T[_]]:
    def forEach[A](t: T[A])(f: A => Unit): Unit

  given Traversable[Sequence] with
    def forEach[A](seq: Sequence[A])(f: A => Unit): Unit = seq match
      case Cons(h, t) => f(h); forEach(t)(f)
      case _ => ()

  given Traversable[Optional] with
    def forEach[A](opt: Optional[A])(f: A => Unit): Unit = opt match
      case Optional.Just(v) => f(v)
      case _ => ()

  def log[A](a: A): Unit = println("The next element is: " + a)

  def logAll[T[_], A](t: T[A])(using trav: Traversable[T]): Unit = trav.forEach(t)(log)

  @main def traversableMain(): Unit =
    val seq = Cons(10, Cons(20, Cons(30, Nil())))
    val opt: Optional[Int] = Optional.Just(30)

    println("Test logAll of Sequence, expected 'The next element is: 10, 20, 30'")
    logAll(seq)

    println("Test logAll of Optional, expected 'The next element is: 30'")
    logAll(opt)