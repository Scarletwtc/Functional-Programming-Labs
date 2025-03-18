import scala.annotation.tailrec


def foldWith(b: Int)(op: (Int, Int) => Int)(start: Int, stop: Int): Int = {
  @tailrec
  def tail_fold(crt: Int, acc: Int): Int = {
    if (crt > stop) acc
    else tail_fold(crt + 1, op(acc, crt))
  }
  tail_fold(start, b)
}
val x = foldWith(0)(_ + _)(1, 3)
println(x)

def foldRight(b: Int)(op: (Int, Int)=> Int)(start: Int, stop: Int): Int = {
  @tailrec
  def tail_fold(crt: Int, acc: Int): Int = {
    if (crt<start) acc
    else tail_fold(crt-1, op(crt, acc))
  }
  tail_fold(stop, 0)
}

var y = foldRight(0)((x,y) => x+y)
print(y(0,10))


def foldConditional(b:Int)(op: (Int, Int) => Int, p:Int => Boolean)(start: Int, stop: Int):Int = {
  @tailrec
  def tail_fold(crt: Int, acc:Int): Int ={
    if(crt>stop) acc
    else if(p(crt)) tail_fold(crt+1, op(acc,crt))
    else tail_fold(crt+1, acc)
  }
  tail_fold(start, b)
}

val sumEvenNumbers = foldConditional(0)(_ + _, _ % 2 == 0)(1, 5)
println(sumEvenNumbers)




def foldMap(op: (Int,Int) => Int, f: Int => Int)(start: Int, stop: Int): Int = {
  @tailrec
  def tail_fold(crt: Int, acc: Int): Int = {
    if (crt>stop) acc
    else tail_fold(crt+1, op(acc, f(crt)))
  }
  tail_fold(start+1, f(start))
}


val result = foldMap(_ + _, (x: Int) => x * x)(1, 3)
println(result)

def sumSquares(n:Int): Int ={
  foldMap((x,y)=> x + y, (x:Int) => x * x)(1,n)
}

val res = sumSquares(3)

def hasDivisor(k: Int, start: Int, stop: Int): Boolean = {
  foldMap(_ + _, (x: Int) => if (x % k == 0) 1 else 0)(start, stop) > 0
}

val result6 = hasDivisor(3, 1, 10)
println(result6)



def integrate(f: Double => Double)(start: Double, stop: Double): Double = {
  if (stop - start < 0.1) {
    (f(start) + f(stop)) * (stop - start) / 2
  } else {
    val mid = (start + stop) / 2
    integrate(f)(start, mid) + integrate(f)(mid, stop)
  }
}

val result7 = integrate((x: Double) => x)(0, 1)
println(result7)



type Line2D = Int => Int
def translateOx(offset: Int)(l: Line2D): Line2D = {
  def newLine( x: Int): Int = l(x) + offset
  newLine
}

val line: Line2D = x => x + 1
val translatedLine: Line2D = translateOx(2)(line)
println(translatedLine(2))

def translateOy(offset: Int)(l: Line2D): Line2D = {
  def newLine(x: Int): Int = l(x-offset)
  newLine
}

val translatedLine2 = translateOy(3)(line)
println(translatedLine2(5))


def intersect(l1: Line2D, l2: Line2D)(start: Int, stop: Int): Boolean = {
  @tailrec
  def tail_intersect(x: Int): Boolean = {
    if (x > stop) false
    else if (l1(x) == l2(x)) true
    else tail_intersect(x + 1)
  }
  tail_intersect(start)

}
val line2: Line2D = x => 2 * x        
val line3: Line2D = x => x+10
val testIntersect1 = intersect(line, line2)(0, 5)
val testIntersect2 = intersect(line, line3)(0, 5)


def larger(l1: Line2D, l2: Line2D)(start: Int, stop: Int): Boolean = {
  @tailrec
  def tail_larger(x: Int): Boolean = {
    if (x > stop) true
    else if (l1(x) > l2(x)) tail_larger(x + 1)
    else false
  }
  tail_larger(start)
}
val testLarger1 = larger(line3, line)(0, 5)
val testLarger2 = larger(line, line3)(0, 5)



