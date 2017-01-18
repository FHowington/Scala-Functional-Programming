package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if(c==0) 1 else pascal(c-1,r-1) + (if(c==r) 0 else pascal(c,r-1))
  /**
   * Exercise 2: Checking to make sure that the parenthesis are balanced and legal, i.e )( is illegal but
    * balanced and () is legal and balanced, while :) is unbalanced.
   */
    def balance(chars: List[Char]): Boolean = {
      def sumList(count: Int, chars: List[Char]): Int = {
        if (count < 0) -1
        else if (chars.isEmpty) 0 else (if (chars.head == '(') 1 else if (chars.head == ')') -1 else 0) + sumList(count + (if (chars.isEmpty) 0 else (if (chars.head == '(') 1 else if (chars.head == ')') -1 else 0)), chars.tail)
      }
      if(sumList(0,chars)==0)true else false
    }
  /**
   * Exercise 3: Determining number of ways to make change given amount of money, and denominations of coins.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if(money==0) 1 else (if(!coins.isEmpty && money >0) countChange(money-coins.head,coins) + countChange(money,coins.tail) else 0)
  }


  }
