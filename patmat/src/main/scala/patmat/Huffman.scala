package patmat

import common._

/**
  * Assignment 4: Huffman coding
  *
  */
object Huffman {
  /**
    * A huffman code is represented by a binary tree.
    *
    * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
    * The weight of a `Leaf` is the frequency of appearance of the character.
    *
    * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
    * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
    * leaves.
    */
  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree


  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Leaf(char, weight) => weight
    case Fork(left, right, chars, weight) => weight
  } // tree match ...

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(char, weight) => List(char)
    case Fork(left, right, chars, weight) => chars
  }

  def left(tree: CodeTree): CodeTree = tree match {
    case Leaf(char, weight) => Leaf(char, weight)
    case Fork(left, right, chars, weight) => left
  }

  def right(tree: CodeTree): CodeTree = tree match {
    case Leaf(char, weight) => Leaf(char, weight)
    case Fork(left, right, chars, weight) => right
  }

  // tree match ...

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  // Part 2: Generating Huffman trees

  /**
    * In this assignment, we are working with lists of characters. This function allows
    * you to easily create a character list from a given string.
    */
  def string2Chars(str: String): List[Char] = str.toList

/** Iterated the character count at the given location */
  def iterateByOne(charcount: List[(Char, Int)], num: Int): List[(Char, Int)] =
    if (num == 0) (charcount.head._1, charcount.head._2 + 1) :: charcount.tail
    else charcount.head :: iterateByOne(charcount.tail, num - 1)

/** Finds the location of the given character. If the character cannot be found, returns -1 */
  def findLocation(charcount: List[(Char, Int)], char: Char, loc: Int): Int =
    if (charcount.isEmpty) -1
    else if (charcount.head._1 == char) loc
    else findLocation(charcount.tail, char, loc + 1)

  /**
    * This function computes for each unique character in the list `chars` the number of
    * times it occurs. For example, the invocation
    *
    * times(List('a', 'b', 'a'))
    *
    * should return the following (the order of the resulting list is not important):
    *
    * List(('a', 2), ('b', 1))
    */
  def times(chars: List[Char]): List[(Char, Int)] = {
    def timesiter(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {
      if (chars.isEmpty) acc
      else if (findLocation(acc, chars.head, 0) == -1) timesiter(chars.tail, (chars.head, 1) :: acc)
      else timesiter(chars.tail, iterateByOne(acc, findLocation(acc, chars.head, 0)))
    }
    timesiter(chars, Nil)
  }


/** Inserts a value into a CodeTree list. Insert location based on weight. Used to update the order **/
  def insert(x: CodeTree, xs: List[CodeTree]): List[CodeTree] = xs match {
    case List() => List(x)
    case y :: ys => if (weight(x) <= weight(y)) x :: xs else y :: insert(x, ys)
  }

  /** Orders list of type CodeTree by accessing the weight value of both types of CodeTree **/
  def makeOrderedList(freqs: List[CodeTree]): List[CodeTree] = {
    def sorter(list: List[CodeTree], acc: List[CodeTree]): List[CodeTree] = {
      if (list.isEmpty) acc
      else sorter(list.tail, insert(list.head, acc))
    }
    sorter(freqs, Nil)
  }

/** Converts list of (Char,Int) to list of Leaf. */
  def convertToLeafs(orders: List[(Char, Int)], acc: List[(Leaf)]): List[Leaf] =
    if (orders.isEmpty) acc
    else convertToLeafs(orders.tail, Leaf(orders.head._1, orders.head._2) :: acc)

  /**
    * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
    *
    * The returned list should be ordered by ascending weights (i.e. the
    * head of the list should have the smallest weight), where the weight
    * of a leaf is the frequency of the character.
    */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[CodeTree] = {
    makeOrderedList(convertToLeafs(freqs, Nil))
  }

  /**
    * Checks whether the list `trees` contains only one single code tree.
    */
  def singleton(trees: List[CodeTree]): Boolean =
    trees.length == 1

  /**
    * The parameter `trees` of this function is a list of code trees ordered
    * by ascending weights.
    *
    * This function takes the first two elements of the list `trees` and combines
    * them into a single `Fork` node. This node is then added back into the
    * remaining elements of `trees` at a position such that the ordering by weights
    * is preserved.
    *
    * If `trees` is a list of less than two elements, that list should be returned
    * unchanged.
    */

  def combine(trees: List[CodeTree]): List[CodeTree] =
    if (trees.length < 2) trees
    else insert(Fork(trees.head, trees.tail.head, chars(trees.head) ::: chars(trees.tail.head), weight(trees.head) + weight(trees.tail.head)), trees.tail.tail)

  /**
    * This function will be called in the following way:
    * until(singleton, combine)(trees)
    * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
    * the two functions defined above.
    */
  def until(single: List[CodeTree] => Boolean, comb: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
    if (single(trees)) trees
    else until(single, comb)(comb(trees))

  /**
    * This function creates a code tree which is optimal to encode the text `chars`.
    * The parameter `chars` is an arbitrary text. This function extracts the character
    * frequencies from that text and creates a code tree based on them.
    */
  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }


  // Part 3: Decoding

  type Bit = Int

  /**
    * This function decodes the bit sequence `bits` using the code tree `tree` and returns
    * the resulting list of characters.
    */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def iter(tree2: CodeTree, bitz: List[Bit], acc: List[Char]): List[Char] = {
      (bitz, tree2) match {
        case (Nil, tree2: Leaf) =>acc :+ tree2.char
        case (rest, tree2: Leaf) => iter(tree, rest, acc :+ tree2.char)
        case (1 :: rest, tree2: Fork) => iter(tree2.right, rest, acc)
        case (0 :: rest, tree2: Fork) => iter(tree2.left, rest, acc)
      }
    }
    iter(tree, bits, Nil)
  }


  /**
    * A Huffman coding tree for the French language.
    * Generated from the data given at
    * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
    */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
    * What does the secret message say? Can you decode it?
    */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
    * A function that returns the decoded secret
    */
  def decodedSecret: List[Char] =
    decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

/** Determines whether a char is located in a CodeTree */
  def contains(x: CodeTree, char: Char): Boolean =
    x match {
      case (x: Fork) => x.chars.contains(char)
      case (x: Leaf) => x.char == char
    }

  /**
    * This function encodes `text` using the code tree `tree`
    * into a sequence of bits.
    */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def iter(remtree: CodeTree, textrem: List[Char], acc: List[Int]): List[Int] = {
      if (textrem.isEmpty) acc
      else remtree match {
        case (remtree: Leaf) => iter(tree, textrem.tail, acc)
        case (remtree: Fork) => if (contains(remtree.left, textrem.head)) iter(remtree.left, textrem, acc :+ 0) else iter(remtree.right, textrem, acc :+ 1)
      }
    }
    iter(tree, text, Nil)
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
    * This function returns the bit sequence that represents the character `char` in
    * the code table `table`.
    */
  def codeBits(table: CodeTable)(char: Char): List[Bit] =
    if (table.head._1 == char) table.head._2
    else codeBits(table.tail)(char)


/** Returns location of char in codetable */
  def findLocationCodeTable(charcount: CodeTable, char: Char, loc: Int): Int =
    if (charcount.isEmpty) -1
    else if (charcount.head._1 == char) loc
    else findLocationCodeTable(charcount.tail, char, loc + 1)

/** Used by IterAll to append onto Code Table **/
  def conIter(charcount: CodeTable, num: Int, nextNum: Int): CodeTable =
    if (num == 0) (charcount.head._1, charcount.head._2 :+ nextNum) :: charcount.tail
    else charcount.head :: conIter(charcount.tail, num - 1, nextNum)

  /** Appends given num onto end of all input chars in Code Table */
  def iterall(charToIter: List[Char], charcount: CodeTable, nextnum: Int): CodeTable = {
    if (charToIter.isEmpty) charcount
    else iterall(charToIter.tail, conIter(charcount, findLocationCodeTable(charcount, charToIter.head, 0), nextnum), nextnum)
  }

  /** Begins conversion of CodeTree to Code table by producing empty Code table with all chars in Code Tree */
  def convertstart(chars: List[Char], acc: CodeTable): CodeTable = {
    if (chars.isEmpty) acc
    else convertstart(chars.tail, (chars.head, Nil) :: acc)
  }

  /**
    * Given a code tree, create a code table which contains, for every character in the
    * code tree, the sequence of bits representing that character.
    */
  def convert(tree: CodeTree): CodeTable = {
    def internal(tre: CodeTree, acc: CodeTable): CodeTable = {
      tre match {
        case tre: Leaf => acc
        case tre: Fork => internal(tre.left, iterall(chars(tre.left), internal(tre.right, iterall(chars(tre.right), acc, 1)), 0))
      }
    }
    internal(tree, convertstart(chars(tree), Nil))
  }

/** Used by quickEncode to find the bit values for chars from a Code Table */
  def returnBits(char: Char, codes: CodeTable): List[Int] =
    if (codes.head._1 == char) codes.head._2
    else returnBits(char, codes.tail)

  /**
    * This function encodes `text` according to the code tree `tree`.
    * To speed up the encoding process, it first converts the code tree to a code table
    * and then uses it to perform the actual encoding.
    */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codes = convert(tree)
    def accumulator(textRemaining: List[Char], acc: List[Bit]): List[Bit] = {
      if (textRemaining.isEmpty) acc
      else accumulator(textRemaining.tail, acc ::: returnBits(textRemaining.head, codes))
    }
    accumulator(text, Nil)
  }
}
