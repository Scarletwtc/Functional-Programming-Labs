import java.awt.event.ItemListener

trait IList
case object Void extends IList
case class Cons(x: Int, xs: IList) extends IList

def isEmpty(l: IList): Boolean =
  l match
    case Void => true
    case Cons(_,_) => false


def size(l: IList): Int =
  l match
    case Void => 0
    case Cons(x, xs) => 1 + size(xs)


def contains(e: Int, l: IList): Boolean =
  l match
    case  Void => false
    case Cons(x, xs) => if(x==e) true else contains(e, xs)


def max(l: IList): Int ={
  def helper(max: Int, lst: IList): Int =
    lst match
      case Void => max
      case Cons(x, xs) => helper(Math.max(x,max),xs)
  helper(Integer.MIN_VALUE, l)
}

def take(n: Int)(l: IList): IList ={
  l match
    case Void => Void
    case Cons(x, xs)  if(n>0) => Cons(x, take(n-1)(xs))
    case _ => Void
}

def drop(n: Int)(l: IList): IList ={
  l match
    case Void => Void
    case Cons(x,xs) if(n>0)=> drop(n-1)(xs)
    case _ => l
}

def append(l1: IList, l2: IList): IList = {
  l1 match {
    case Void => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))
  }
}

def last(l: IList): Int =
  l match
    case Void => -1
    case Cons(x, Void) => x
    case Cons(_, xs) => last(xs)

def reverse(l: IList): IList =
  l match
    case Void => Void
    case Cons(x, xs) => append(reverse(xs), Cons(x, Void))


def reverse2(l: IList): IList = {
  def loop(remaining: IList, acc: IList): IList =
    remaining match
      case Void =>acc
      case Cons(x, xs) => loop(xs, Cons(x,acc))
  loop(l,Void)
}

def isSorted(l: IList): Boolean =
  l match
    case Void => true
    case Cons(x, Void) => true
    case Cons(x1, Cons(x2, xs)) => if(x1 < x2) isSorted(Cons(x2, xs)) else false


// def merge si def mergesort sunt in curs 4