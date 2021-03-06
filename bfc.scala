// Part 2 about a "Compiler" for the Brainf*** language
//======================================================

// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}

type Mem = Map[Int, Int]

import io.Source
import scala.util._

// !! COPY from your bf.scala !!

def load_bff(name: String) : String = {
  Try(Source.fromFile(name).mkString).getOrElse("")
}
  
def sread(mem: Mem, mp: Int) : Int = {
  mem.getOrElse(mp,0)
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
   mem + (mp->v)
}

def jumpRight(prog: String, pc: Int, level: Int) : Int = {
  val length = prog.length
  val substr = prog.substring(pc)
  val indexClose = substr.indexOf(']')
  val indexOpen = substr.indexOf('[')
  if (indexClose == -1) {
    length
  }
  else {
      if (level == 0 && (indexClose < indexOpen || indexOpen == -1)){
        length - substr.length + indexClose + 1
      }
      else if (level != 0 && (indexClose < indexOpen || indexOpen == -1)) {
        jumpRight(prog, length - substr.length +indexClose + 1, level-1)
      }
      else {
        jumpRight(prog, length - substr.length + indexOpen + 1, level+1)
      }
  }

}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = {
  val length = prog.length
  val substr = prog.substring(0,pc+1)
  val indexClose = substr.lastIndexOf(']')
  val indexOpen = substr.lastIndexOf('[')
  if (indexOpen == -1) {
    -1
  }
  else {
      if (level == 0 && (indexOpen > indexClose || indexClose == -1)){
        indexOpen + 1
      }
      else if (level != 0 && (indexOpen > indexClose || indexClose == -1)) {
        jumpLeft(prog, indexOpen-1, level-1)
      }
      else {
        jumpLeft(prog, indexClose-1, level+1)
      }
  }
}

def my_lift(prog: String, pc : Int) : Char = {
  Try(prog(pc)).getOrElse('\u0000')
}
def increment_if_exists(mp: Int, mem: Mem) : Mem = {
  if (mem.get(mp) == None){
    mem + (mp->1)
  }
  else {
    mem + (mp -> (mem(mp) + 1))
  }
}
def decrement_if_exists(mp: Int, mem: Mem) : Mem = {
  if (mem.get(mp) == None){
    mem + (mp->(-1))
  }
  else {
    mem + (mp -> (mem(mp) - 1))
  }
}
def read_if_exists(mp: Int, mem: Mem) : Int = {
  if (mem.get(mp) == None){
    0
  }
  else {
    mem(mp)
  }
}

def compute(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = my_lift(prog,pc) match {
  case ('\u0000') => mem
  case ('>') => compute(prog,pc+1,mp+1,mem)
  case ('<') => compute(prog,pc+1,mp-1,mem)
  case ('+') => compute(prog,pc+1,mp,increment_if_exists(mp,mem))
  case ('-') => compute(prog,pc+1,mp,decrement_if_exists(mp,mem))
  case ('.') => {
    print(mem(mp).toChar)
    compute(prog,pc+1,mp,mem)
  }
  case (',') => compute(prog,pc+1,mp,mem + (mp->Console.in.read().toByte))
  case ('[') => {
    if (read_if_exists(mp,mem) == 0){
      compute(prog,jumpRight(prog, pc + 1, 0),mp,mem)
    }
    else{
      compute(prog,pc +1,mp,mem)
    }
  }
  case (']') => {
    if (read_if_exists(mp,mem) != 0){
      compute(prog,jumpLeft(prog, pc - 1, 0),mp,mem)
    }
    else{
      compute(prog,pc +1,mp,mem)
    }
  }
  case _ => compute(prog,pc +1,mp,mem)
}

def run(prog: String, m: Mem = Map()) = {
  compute(prog,0,0,m)
}

// The baseline to what we can compare our "compiler"
// implemented below. It should require something like 
// 60 seconds for the calculation on my laptop
//
//time_needed(1, run(load_bff("benchmark.bf")))


// DEBUGGING INFORMATION!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. The point is that for example for
// the Sierpinski program, they need to still generate code
// that displays such a triangle. If yes, then one usually
// can take comfort that all is well. If not, then something
// went wrong during the optimisations.


// ADVANCED TASKS
//================

// (5) Write a function jtable that precomputes the "jump
//     table" for a bf-program. This function takes a bf-program 
//     as an argument and Returns a Map[Int, Int]. The 
//     purpose of this map is to record the information
//     that given on the position pc is a '[' or a ']',
//     then to which pc-position do we need to jump next?
// 
//     For example for the program
//    
//       "+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]"
//
//     we obtain the map
//
//       Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)
//  
//     This states that for the '[' on position 5, we need to
//     jump to position 20, which is just after the corresponding ']'.
//     Similarly, for the ']' on position 19, we need to jump to
//     position 6, which is just after the '[' on position 5, and so
//     on. The idea is to not calculate this information each time
//     we hit a bracket, but just look up this information in the 
//     jtable. You can use the jumpLeft and jumpRight functions
//     from Part 1 for calculating the jtable.
//
//     Then adapt the compute and run functions from Part 1 in order 
//     to take advantage of the information stored in the jtable. 
//     This means whenever jumpLeft and jumpRight was called previously,
//     you should look up the jump address in the jtable.
 

def jtable(pg: String) : Map[Int, Int] = {
  val pgIndex = pg.toList.zipWithIndex
  val openMap = (for (c <- pgIndex if (c._1 == '[')) yield (c._2,jumpRight(pg,c._2 + 1,0))).toMap 
  val closeMap = (for (c <- pgIndex if (c._1 == ']')) yield (c._2,jumpLeft(pg,c._2 - 1,0))).toMap 
  openMap ++ closeMap
}

// testcase
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = my_lift(pg,pc) match {
  case ('\u0000') => mem
  case ('>') => compute2(pg,tb,pc+1,mp+1,mem)
  case ('<') => compute2(pg,tb,pc+1,mp-1,mem)
  case ('+') => compute2(pg,tb,pc+1,mp,increment_if_exists(mp,mem))
  case ('-') => compute2(pg,tb,pc+1,mp,decrement_if_exists(mp,mem))
  case ('.') => {
    print(mem(mp).toChar)
    compute2(pg,tb,pc+1,mp,mem)
  }
  case (',') => compute2(pg,tb,pc+1,mp,mem + (mp->Console.in.read().toByte))
  case ('[') => {
    if (read_if_exists(mp,mem) == 0){
      compute2(pg,tb,tb(pc),mp,mem)
    }
    else{
      compute2(pg,tb,pc +1,mp,mem)
    }
  }
  case (']') => {
    if (read_if_exists(mp,mem) != 0){
      compute2(pg,tb,tb(pc),mp,mem)
    }
    else{
      compute2(pg,tb,pc +1,mp,mem)
    }
  }
  case _ => compute2(pg,tb,pc +1,mp,mem)
}

def run2(pg: String, m: Mem = Map()) = {
  compute2(pg,jtable(pg),0,0,m)
}


//testcase
//time_needed(1, run2(load_bff("benchmark.bf")))



// (6) Write a function optimise which deletes "dead code" (everything
// that is not a bf-command) and also replaces substrings of the form
// [-] by a new command 0. The idea is that the loop [-] just resets the
// memory at the current location to 0. In the compute3 and run3 functions
// below you implement this command by writing the number 0 to mem(mp), 
// that is write(mem, mp, 0). 
//
// The easiest way to modify a string in this way is to use the regular
// expression """[^<>+-.,\[\]]""", which recognises everything that is 
// not a bf-command and replace it by the empty string. Similarly the
// regular expression """\[-\]""" finds all occurrences of [-] and 
// by using the Scala method .replaceAll you can replace it with the 
// string "0" standing for the new bf-command.

def optimise(s: String) : String = {
  s.replaceAll("""[^<>+-.,\[\]]""","").replaceAll("""\[-\]""","0")
}

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = my_lift(pg,pc) match {
  case ('\u0000') => mem
  case ('>') => compute3(pg,tb,pc+1,mp+1,mem)
  case ('<') => compute3(pg,tb,pc+1,mp-1,mem)
  case ('+') => compute3(pg,tb,pc+1,mp,increment_if_exists(mp,mem))
  case ('-') => compute3(pg,tb,pc+1,mp,decrement_if_exists(mp,mem))
  case ('.') => {
    print(mem(mp).toChar)
    compute3(pg,tb,pc+1,mp,mem)
  }
  case (',') => compute3(pg,tb,pc+1,mp,mem + (mp->Console.in.read().toByte))
  case ('[') => {
    if (read_if_exists(mp,mem) == 0){
      compute3(pg,tb,tb(pc),mp,mem)
    }
    else{
      compute3(pg,tb,pc +1,mp,mem)
    }
  }
  case (']') => {
    if (read_if_exists(mp,mem) != 0){
      compute3(pg,tb,tb(pc),mp,mem)
    }
    else{
      compute3(pg,tb,pc +1,mp,mem)
    }
  }
  case ('0') => {
    compute3(pg,tb,pc+1,mp,write(mem,mp,0))
  }
  case _ => compute3(pg,tb,pc +1,mp,mem)
}

def run3(pg: String, m: Mem = Map()) = {
  val opt = optimise(pg)
  compute3(opt,jtable(opt),0,0,m)
}


// testcases

//optimise(load_bff("benchmark.bf"))          // should have inserted 0's
//optimise(load_bff("mandelbrot.bf")).length  // => 11203
 
//time_needed(1, run3(load_bff("benchmark.bf")))



// (7)  Write a function combine which replaces sequences
// of repeated increment and decrement commands by appropriate
// two-character commands. For example for sequences of +
//
//              orig bf-cmds  | replacement
//            ------------------------------
//              +             | +A 
//              ++            | +B
//              +++           | +C
//                            |
//              ...           |
//                            | 
//              +++....+++    | +Z
//                (where length = 26)
//
//  Similar for the bf-command -, > and <. All other commands should
//  be unaffected by this change.
//
//  Adapt the compute4 and run4 functions such that they can deal
//  appropriately with such two-character commands.

def consec_items[T](list: List[T]) : List[List[T]] = list match {
  case Nil => Nil
  case head::tail => {
    val items = list.takeWhile(_ == head)
    items :: consec_items(list.drop(items.length))
  }
}

def unpack[T](list: List[List[List[T]]], acc: List[List[T]]) : List[List[T]] = list match { 
  case Nil => acc
  case _ => {
    val head = list.head
    val elements = (for (lst <- head) yield lst).toList
    unpack(list.tail,acc:::elements)
  }
}

def combine(s: String) : String = {
  val combineOps = List('<','>','+','-')
  val lists = consec_items(s.toList)
  val newList = for (lst <- lists) yield (lst.grouped(26).toList)
  val sizes = for (lst <- unpack(newList,Nil)) yield (if(combineOps.contains(lst.head)){(lst.head,(lst.size + 64).toChar)} else (lst.mkString,""))
  sizes.flatMap{ case (a,b) => List(a,b)  }.mkString
}

// testcase
//combine(load_bff("benchmark.bf"))
def add_if_exists(mp: Int, mem: Mem, amount: Int) : Mem = {
  if (mem.get(mp) == None){
    mem + (mp->amount)
  }
  else {
    mem + (mp -> (mem(mp) + amount))
  }
}
def decrease_if_exists(mp: Int, mem: Mem, amount: Int) : Mem = {
  if (mem.get(mp) == None){
    mem + (mp->(amount*(-1)))
  }
  else {
    mem + (mp -> (mem(mp) - amount))
  }
}
def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = my_lift(pg,pc) match {
  case ('\u0000') => mem
  case ('>') => {
    val amount = (my_lift(pg,pc+1) - 64).toInt
    compute4(pg,tb,pc+2,mp+amount,mem)
  }
  case ('<') => {
    val amount = (my_lift(pg,pc+1) - 64).toInt
    compute4(pg,tb,pc+2,mp-amount,mem)
  }
  case ('+') => {
    val amount = (my_lift(pg,pc+1) - 64).toInt
    compute4(pg,tb,pc+2,mp,add_if_exists(mp,mem,amount))
  }
  case ('-') => {
    val amount = (my_lift(pg,pc+1) - 64).toInt
    compute4(pg,tb,pc+2,mp,decrease_if_exists(mp,mem,amount))
  }
  case ('.') => {
    print(mem(mp).toChar)
    compute4(pg,tb,pc+1,mp,mem)
  }
  case (',') => compute4(pg,tb,pc+1,mp,mem + (mp->Console.in.read().toByte))
  case ('[') => {
    if (read_if_exists(mp,mem) == 0){
      compute4(pg,tb,tb(pc),mp,mem)
    }
    else{
      compute4(pg,tb,pc +1,mp,mem)
    }
  }
  case (']') => {
    if (read_if_exists(mp,mem) != 0){
      compute4(pg,tb,tb(pc),mp,mem)
    }
    else{
      compute4(pg,tb,pc +1,mp,mem)
    }
  }
  case ('0') => {
    compute4(pg,tb,pc+1,mp,write(mem,mp,0))
  }
  case _ => compute4(pg,tb,pc +1,mp,mem)
}


// should call first optimise and then combine on the input string
def run4(pg: String, m: Mem = Map()) = {
  val opt = optimise(pg)
  val com = combine(opt)
  compute4(com,jtable(com),0,0,m)
}


// testcases
//combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

//time_needed(1, run4(load_bff("benchmark.bf")))

//time_needed(1, run(load_bff("sierpinski.bf"))) 
//time_needed(1, run4(load_bff("sierpinski.bf"))) 

//time_needed(1, run4(load_bff("mandelbrot.bf")))


