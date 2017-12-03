import scala.io.Source

val numbers = Source.fromFile("./data/day02.txt").getLines
  .map(line => line.split("\t").map(_.toInt))
  .toArray

println("Part I : " + numbers.foldLeft(0)((checksum, line) => {
  checksum + (line.reduceLeft(_ max _) - line.reduceLeft(_ min _))
}))

println("Part II: " + numbers.foldLeft(0)((checksum: Int, line) => {
  checksum + line
    .map((n1: Int) => {
      line
        .map((n2: Int) => n1.toFloat / n2.toFloat)
        .find(s => s != 1 && s % 1 == 0)
        .getOrElse(0f).toInt
    }: Int)
    .find(_ != 0)
    .getOrElse(0)
}))
