
/**
  * Monad operations
  * List could be replaced by every type of Monad
  *
  */
def unit[A](a: A): List[A] = List(a)

def flatMap[A, B](a: List[A])(fn: A => List[B]): List[B] = a.flatMap(el => fn(el))

def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(el => f(el))


// Implementation example
val list = List(1,2,3,4)
map(list)(_ * 4)

val list2 = List(List(1,2,3,4))
val fn2 = (l: List[Int]) => l.map(x => x * 10)
// could also impl this function: val fn = (el: Int) => List(el * 10)
flatMap(list2)(fn2)

unit(1)

val one = Option(1)
val twentyTwo = one.map(innerValue => (innerValue * 22).toString)

val number1: Option[Int] = Option(10)
val number2: Option[Int] = Option(20)
val mapResult: Option[Option[Int]] = number1.map(x => number2.map(y => y + x))
val flatMapResult: Option[Int] = number1.flatMap(x => number2.map(y => y + x))

