// Part 1 about an Interpreter for the Brainf*** language
//========================================================


// representation of Bf memory 

type Mem = Map[Int, Int]


// (1) Write a function that takes a file name as argument and
// and requests the corresponding file from disk. It Returns the
// content of the file as a String. If the file does not exists,
// the function should Return the empty string.

import io.Source
import scala.util._

def load_bff(name: String) : String = {
  Try(Source.fromFile(name).mkString).getOrElse("")
}



// (2) Complete the functions for safely reading  
// and writing brainf*** memory. Safely read should
// Return the value stored in the Map for a given memory
// pointer, provided it exists; otherwise it Returns 0. The
// writing function generates a new Map with the
// same data, except at the given memory pointer the
// value v is stored.


def sread(mem: Mem, mp: Int) : Int = {
  mem.getOrElse(mp,0)
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
   mem + (mp->v)
}



// (3) Implement the two jumping instructions in the 
// brainf*** language. In jumpRight, given a program and 
// a program counter move the program counter to the right 
// until after the *matching* ]-command. Similarly, 
// jumpLeft implements the move to the left to just after
// the *matching* [-command.

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


// testcases
//jumpRight("""--[..+>--],>,++""", 3, 0)         // => 10
//jumpLeft("""--[..+>--],>,++""", 8, 0)          // => 3
//jumpRight("""--[..[+>]--],>,++""", 3, 0)       // => 12
//jumpRight("""--[..[[-]+>[.]]--],>,++""", 3, 0) // => 18
//jumpRight("""--[..[[-]+>[.]]--,>,++""", 3, 0)  // => 22 (outside)
//jumpLeft("""[******]***""", 7, 0)              // => -1 (outside)



// (4) Complete the compute function that interprets (runs) a brainf***
// program: the arguments are a program (represented as a string), a program 
// counter, a memory counter and a brainf*** memory. It Returns the
// memory at the stage when the execution of the brainf*** program
// finishes. The interpretation finishes once the program counter
// pc is pointing to something outside the program string.
// If the pc points to a character inside the program, the pc, 
// memory pointer and memory need to be updated according to 
// rules of the brainf*** language. Then, recursively, the compute 
// function continues with the command at the new program
// counter. 
//
// Implement the run function that calls compute with the program
// counter and memory counter set to 0.

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


/*

// some sample bf-programs collected from the Internet
//=====================================================


// first some contrived (small) programs

// clears the 0-cell
run("[-]", Map(0 -> 100))    // Map will be 0 -> 0

// copies content of the 0-cell to 1-cell
run("[->+<]", Map(0 -> 10))  // Map will be 0 -> 0, 1 -> 10


// copies content of the 0-cell to 2-cell and 4-cell
run("[>>+>>+<<<<-]", Map(0 -> 42))


// prints out numbers 0 to 9
run("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")


// some more "useful" programs

// hello world program 1
run("""++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++
       ..+++.>>.<-.<.+++.------.--------.>>+.>++.""")

// hello world program 2
run("""++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>+
      +.<<+++++++++++++++.>.+++.------.--------.>+.>.""")


// draws the Sierpinski triangle
run("""++++++++[>+>++++<<-]>++>>+<[-[>>+<<-]+>>]>+[-<<<[
      ->[+[-]+>++>>>-<<]<[<]>>++++++[<<+++++>>-]+<<++.[-]<<
      ]>.>+[>>]>+]""")

run(load_bff("sierpinski.bf"))


//fibonacci numbers below 100
run("""+++++++++++
      >+>>>>++++++++++++++++++++++++++++++++++++++++++++
      >++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>
      +<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-
      <-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<
      -]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]
      >[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++++++++++++++++
      +++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++
      ++++++++++++++++++++++++++++++++++++++++++++.[-]<<
      <<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<
      [-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]""")


//outputs the square numbers up to 10000
run("""++++[>+++++<-]>[<+++++>-]+<+[
    >[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
    >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
    <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]""")

//collatz numbers (needs a number to be typed in)
run(""">,[[----------[
      >>>[>>>>]+[[-]+<[->>>>++>>>>+[>>>>]++[->+<<<<<]]<<<]
      ++++++[>------<-]>--[>>[->>>>]+>+[<<<<]>-],<]>]>>>++>+>>[
      <<[>>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<<]]<[>+<-]>]
      >[>[>>>>]+[[-]<[+[->>>>]>+<]>[<+>[<<<<]]+<<<<]>>>[->>>>]+>+[<<<<]]
      >[[>+>>[<<<<+>>>>-]>]<<<<[-]>[-<<<<]]>>>>>>>
      ]>>+[[-]++++++>>>>]<<<<[[<++++++++>-]<.[-]<[-]<[-]<]<,]""")


// infinite collatz (never stops)
run(""">>+>+<[[->>[>>]>>>[>>]+[<<]<<<[<<]>[>[>>]>>+>[>>]<+<[<<]<<<[<
      <]>-]>[>>]>>[<<<<[<<]>+>[>>]>>-]<<<<[<<]+>>]<<[+++++[>+++++++
      +<-]>.<++++++[>--------<-]+<<]>>[>>]+[>>>>[<<+>+>-]<-[>+<-]+<
      [<<->>-[<<+>>[-]]]>>>[<<<+<<+>>>>>-]<<<[>>>+<<<-]<<[[-]>+>>->
      [<+<[<<+>>-]<[>+<-]<[>+<-]>>>>-]<[>+<-]+<[->[>>]<<[->[<+++>-[
      <+++>-[<+++>-[<[-]++>>[-]+>+<<-[<+++>-[<+++>-[<[-]+>>>+<<-[<+
      ++>-[<+++>-]]]]]]]]]<[>+<-]+<<]>>>+<[->[<+>-[<+>-[<+>-[<+>-[<
      +>-[<+>-[<+>-[<+>-[<+>-[<[-]>>[-]+>+<<-[<+>-]]]]]]]]]]]<[>+<-
      ]+>>]<<[<<]>]<[->>[->+>]<[-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<-
      >>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>
      -[<->>+<-[<+>-[<->>+<-[<+>-]]]]]]]]]]]]]]]]]]]>[<+>-]<+<[<+++
      +++++++>-]<]>>[<+>->>]<<[>+>+<<-]>[<+>-]+>[<->[-]]<[-<<-]<<[<
      <]]++++++[>+++++++<-]>++.------------.[-]>[>>]<<[+++++[>+++++
      +++<-]>.<++++++[>--------<-]+<<]+<]>[<+>-]<]>>>[>>]<<[>[-]<-<
      <]++++++++++.[-]<<<[<<]>>>+<[->[<+>-[<+>-[<+>-[<+>-[<+>-[<+>-
      [<+>-[<+>-[<+>-[<[-]>>[-]+>+<<-]]]]]]]]]]<[>+<-]+>>]<<[<<]>>]""")



// a Mandelbrot set generator in brainf*** written by Erik Bosman
// (http://esoteric.sange.fi/brainfuck/utils/mandelbrot/)

run(load_bff("mandelbrot.bf"))


// a benchmark program (counts down from 'Z' to 'A')
val b1 = """>++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++
            [>++++++++<-]>.[-]<<>++++++++++[>++++++++++[>++
            ++++++++[>++++++++++[>++++++++++[>++++++++++[>+
            +++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++."""


def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}

time_needed(1, run(b1))
*/

