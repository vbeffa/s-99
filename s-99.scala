// P01
def last[A](ls: List[A]): A = ls match {
  case last :: Nil => last
  case _ :: tail => last(tail)
  case _ => throw new NoSuchElementException
}
