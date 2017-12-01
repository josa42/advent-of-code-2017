import scala.io.Source

val numbers = Source.fromFile("./data/day01.txt").getLines.mkString
  .split("")
  .map((c) => c.toInt);

println(s"Part I : " + numbers.zipWithIndex.foldLeft(0)( (s, value) => {
  val next = numbers((value._2 + 1) % numbers.size)
  if (value._1 == next) s + value._1 else s
}))

println(s"Part II: " + numbers.zipWithIndex.foldLeft(0)( (s, value) => {
  val next = numbers((value._2 + (numbers.size / 2)) % numbers.size)
  if (value._1 == next) s + value._1 else s
}))