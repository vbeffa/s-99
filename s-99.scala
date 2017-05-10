// P01
def last[A](ls: List[A]): A = ls match {
  case last :: Nil => last
  case _ :: tail => last(tail)
  case _ => throw new NoSuchElementException
}

// P02
def penultimate[A](ls: List[A]): A = ls match {
  case penultimate :: ultimate :: Nil => penultimate
  case _ :: tail => penultimate(tail)
  case _ => throw new NoSuchElementException
}

// P03
def nth[A](i: Int, ls: List[A]): A = i match {
  case x if x < 0 => throw new IllegalArgumentException("i must be non-negative")
  case 0 => ls.head
  case i => nth(i - 1, ls.tail)
}

// P04
def length[A](ls: List[A]): Int = ls match {
  case Nil => 0
  case _ :: tail => 1 + length(tail)
}

def length[A](ls: List[A]): Int = ls.foldLeft(0)((acc, _) => acc + 1)

// P05
def reverse[A](ls: List[A]): List[A] = ls match {
  case Nil => Nil
  case head :: tail => reverse(tail) :+ head
}

// P06
def isPalindrome[A](ls: List[A]): Boolean = ls match {
  case Nil => true
  case _ :: Nil => true
  case first :: (middle :+ last) => first == last && isPalindrome(middle)
}
