import collection.mutable

def square(x: Int) = x * x
def abs(x: Int) = if (x < 0) x * -1 else x

def nextPosition(pos: (Int, Int), dir: (Int, Int)) = (pos._1 + dir._1, pos._2 + dir._2)

def turn(pos: (Int, Int), dir: (Int, Int), layerIdx: Int) = {
  val maxDist = layerIdx - 1
  val minDist = maxDist * -1
  
  pos match {
    // start
    case (0, 0) => (1, 0)
    
    // Right, Bottom
    case (`maxDist`, `minDist`) => (-1, 0)
    // Left, Bottom
    case (`minDist`, `minDist`) => (0, 1)
    // Left, Top
    case (`minDist`, `maxDist`) => (1, 0)
    // Right, Top
    case (`maxDist`, `maxDist`) => (1, 0)
    
    // Right Side
    case (`maxDist`, _) => (0, -1)
    // Bottom Side
    case (_, `minDist`) => (-1, 0)
    // Left Side
    case (`minDist`, _) => (0, 1)
    // Top Side
    case (_, `maxDist`) => (1, 0)
    
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
  var dir = (0, 0)
  
  while (i < idx) {
    i += 1
    dir = turn(pos, dir, layerIdx)
    pos = nextPosition(pos, dir)
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
  
  for (y <-  (minDist to maxDist).reverse) {
    for (x <-  minDist to maxDist) {
      // print(s"($x / $y)  ")
      val pos = (x, y)
      if (values.isDefinedAt(pos)) {
        val value = values(pos)
        print("% 7d  ".format(value))
      } else {
        print("         ")
      }
    }
    println("")
  }
}

def getMemory(pos: (Int, Int), values: mutable.Map[(Int, Int), Int]): Int = {
  if (values.isDefinedAt(pos)) values(pos) else 0
}

def nextNumber(pos: (Int, Int), values: mutable.Map[(Int, Int), Int]): Int = {
  
  var number = 0
  
  
  for (x <- -1 to 1) {
    for (y <- -1 to 1) {
      if (!(x == 0 && y == 0)) {
        number += getMemory((pos._1 + x, pos._2 + y), values)
      }
    }
  }
  
  number
}

assert(steps(   1) ==  0, "steps(   1) =  0")
assert(steps(  12) ==  3, "steps(  12) =  3")
assert(steps(  23) ==  2, "steps(  23) =  2")
assert(steps(1024) == 31, "steps(1024) = 31")
assert(steps(325489) == 552, "steps(325489) = 552")

def runPart1() = {
  println(s"Part I : " + steps(325489))
}
def runPart2() = {
  val memory: mutable.Map[(Int, Int), Int] = mutable.Map()
  var idx = 1
  var number = 1
  var pos = (0, 0)
  var dir = (0, 0)

  var continue = true
  // 325489
  while (continue) {
    memory(pos) = number
    val (layerIdx, _) = layer(idx);
    
    // println(s"$idx: Layer $layerIdx")
    // printMemory(layerIdx, memory)
    
    idx += 1
    
    dir = turn(pos, dir, layerIdx)
    pos = nextPosition(pos, dir)
    
    continue = number < 325489
    number = nextNumber(pos, memory)
  }
  
  println(s"Part II: $number")
}



runPart1
runPart2

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