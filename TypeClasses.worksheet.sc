trait Splittable[A]:
	def split(a: A): (A, A)
	def say = "Hey"
	extension(a: A)
		def splitted: (A, A) = split(a)

trait Mergeable[A]:
	def merge(la: A, ra: A): A
	extension(ta: (A, A))
		def merged: A = merge(ta(0), ta(1))

given IntSplittable: Splittable[Int] with
	def split(i: Int) = (i/2 -> (i - i/2))

given IntMergeable: Mergeable[Int] with
	def merge(i: Int, j: Int) = i + j

// equivalent to splitAll[A](l: List[A])(using Splittable[A]): List[(A, A)] = ...
def splitAll[S: Splittable](l : List[S]): List[(S, S)] = l.map(_.splitted)

def mergeAll[M: Mergeable](l: List[(M, M)]): List[M] = l.map(_.merged)

def splitThenShiftMerge[A](acc: List[A], l: List[A])(using Splittable[A])(using Mergeable[A]): List[A] =
	l match
		case Nil => acc.reverse
		case a :: Nil => acc.reverse
		case a :: b :: tail => splitThenShiftMerge((a.splitted(1) -> b.splitted(0)).merged :: acc, b :: tail)

def splitThenShiftMerge[A](l: List[A])(using Splittable[A])(using Mergeable[A]): List[A] = splitThenShiftMerge(List.empty[A], l)

24.splitted // (12, 12)
25.splitted // (12, 13)

splitAll(List(24,25)) // List((12, 12), (12, 13))
mergeAll(List(1 -> 2, 39 -> 3)) // List(3, 42)

splitThenShiftMerge(List(2,8,9,14)) // List(5, 8, 12)
