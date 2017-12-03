import collection.mutable

def square(x: Int) = x * x
def abs(x: Int) = if (x < 0) x * -1 else x

def nextPosition(pos: (Int, Int), dir: (Int, Int)) = (pos._1 + dir._1, pos._2 + dir._2)

def turn(pos: (Int, Int), dir: (Int, Int), layerIdx: Int) = {
  val maxDist = layerIdx - 1
  val minDist = maxDist * -1
  
  pos match {
    // Right, Bottom
    case (`maxDist`, `minDist`) => (-1, 0)
    // Right
    case (`maxDist`, _) => (0, -1)
    // Left, Bottom
    case (`minDist`, `minDist`) => (0, 1)
    // Left, Top
    case (`minDist`, `maxDist`) => (1, 0)
    // No change
    case _ => dir
  }
}

def layer(idx: Int): (Int, Int) = {
  // layer index starts at 1
  var layerIdx: Int = 1
  
  // layer side length
  var layerSide: Int = 1
  
  // total size
  var size: Int = 1
  
  while (size < idx) {
    layerIdx += 1
    layerSide = (layerIdx * 2) - 1
  
    size = square(layerSide)
  }
  
  (layerIdx, layerSide)
}

def relativePosition(idx: Int): (Int, Int) = {
  val (layerIdx, layerSide) = layer(idx);
  
  var pos = (layerIdx - 1, if (layerIdx > 1) layerIdx - 2 else 0)
  
  var i = square(layerSide - 2) + 1
  var dir = (0, -1)
  
  while (i < idx) {
    i += 1
    pos = nextPosition(pos, dir)
    dir = turn(pos, dir, layerIdx)
  }
  
  pos
}

def steps(idx: Int): Int = {
  val pos = relativePosition(idx)
  abs(pos._1) + abs(pos._2)
}


def printMemory(layerIdx: Int, values: mutable.Map[(Int, Int), Int]) = {
  val maxDist = layerIdx - 1
  val minDist = maxDist * -1
  
  for (y <-  minDist to maxDist) {
    for (x <-  minDist to maxDist) {
      // print(s"($x / $y)  ")
      val pos = (x, y)
      if (values.isDefinedAt(pos)) {
        val value = values(pos)
        print("% 3d  ".format(value))
      } else {
        print("     ")
      }
    }
    println("")
  }
}


// for (i:Int <- 1 to 23) println(s"$i => " + steps(i))

assert(steps(   1) ==  0, "steps(   1) =  0")
assert(steps(  12) ==  3, "steps(  12) =  3")
assert(steps(  23) ==  2, "steps(  23) =  2")
assert(steps(1024) == 31, "steps(1024) = 31")

println(s"steps: " + steps(325489))


// println("Layer 1")
// printMemory(1, Map(
//   (0, 0) -> 1
// ))
// println("")

// println("Layer 2")
// printMemory(2, Map(
//   (-1, -1) -> 1,
//   ( 0, -1) -> 2,
//   ( 1, -1) -> 3,
//   (-1,  0) -> 4,
//   ( 0,  0) -> 5,
//   ( 1,  0) -> 6,
//   (-1,  1) -> 7,
//   ( 0,  1) -> 8,
//   ( 1,  1) -> 9
// ))


val memory: mutable.Map[(Int, Int), Int] = mutable.Map()
var idx = 1
var number = 0
var pos = (0, 0)
var dir = (1, 0)

// 325489
while (number <= 12) {
  memory(pos) = number
  val (layerIdx, _) = layer(idx);
  
  
  
  
  println(s"Layer $idx : $layerIdx ($pos => $dir)")
  printMemory(layerIdx, memory)
  
  
  number += 1
  idx += 1
  
  if (idx == 2) {
    dir = (0, 1)
  } else {
    dir = turn(pos, dir, layerIdx)
  }
  pos = nextPosition(pos, dir)
  
  
}

// println("Layer 3")
// printMemory(3)
// 
// println("Layer 4")
// printMemory(4)

/*
Spiral

43 44 45 46 47 48 49
42 21 22 23 24 25 26
41 20  7  8  9 10 27
40 19  6  1  2 11 28
39 18  5  4  3 12 29
38 17 16 15 14 13 30
37 36 35 34 33 32 31

Layer 1

          1


Layer 2

       7  8  9
       6     2 <= Start (1, 0)
       5  4  3


Layer 3

   21 22 23 24 25
   20          10 <= Start (2, 1)
   19          11
   18          12
   17 16 15 14 13


Layer 4

43 44 45 46 47 48 49
42                26 <= Start (3, 2)
41                27
40                28
39                29
38                30
37 36 35 34 33 32 31

 */